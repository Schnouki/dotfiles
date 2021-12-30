import std/[ json, os, parseopt, sequtils, strformat, strutils, tables ]

import ./colors, ./stats


const
  RUNES = @[
    "\u3001", "\u3002", "\u3003", "\u3004", "\u3005", "\u3006", "\u3007", "\u3008"
  ]


proc writeHelp() =
  echo "Usage: bars [OPTION]..."
  echo ""
  echo "Options:"
  echo "  -d:D, --delay:D  wait D seconds between each update"
  echo "  -s:S, --size:S   use an history of size S"
  echo "  -t, --text       run in text mode (instead of graph mode)"


proc runTextMode(delay: int) =
  var st1 = getCpuStats()

  while true:
    sleep(delay)
    let st2 = getCpuStats()
    var line = ""
    for cpu, cst in st2.pairs:
      let usage = usagePct(st1[cpu], cst) * 100.0
      line &= &"{cpu}: {usage:5.1f}%  "
    echo line
    st1 = st2


proc asBar(d: seq[string]): string =
  return "⎹<span font_desc=\"Bars\">" & d.join() & "</span>⎸"


proc getRune(v: float): string =
  let idx = (v * len(RUNES).float).int
  let r = RUNES[idx.clamp(0, len(RUNES)-1)]
  let col = getBarColor(v)
  return &"<span color=\"{col}\">{r}</span>"


proc runGraphMode(delay, size: int) =
  var st1 = getCpuStats()

  let bars = newOrderedTable[string, seq[string]]()
  for cpu in st1.keys:
    bars[cpu] = sequtils.repeat("\u3000", size)

  sleep(500)
  while true:
    let st2 = getCpuStats()
    var tooltip = newSeq[string]()
    for cpu, cst in st2.pairs:
      let usage = usagePct(st1[cpu], cst)
      let bar = bars[cpu][1..^1] & getRune(usage)
      bars[cpu] = bar
      tooltip.add(&"{cpu:5}: {usage * 100.0:5.1f}% " & bar.asBar())

    let j = %* {"text": "" & bars["cpu"].asBar(), "tooltip": "<tt>" & tooltip.join("\n") & "</tt>"}
    echo j

    st1 = st2
    sleep(delay)


proc main() =
  var p = initOptParser(shortNoVal = {'t'}, longNoVal = @["text"])
  var delay = 2
  var size = 15
  var textMode = false

  for kind, key, val in p.getopt():
    case kind
    of cmdArgument:
      writeHelp()
      return

    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        writeHelp()
        return
      of "delay", "d":
        delay = val.parseInt()
      of "size", "s":
        size = val.parseInt()
      of "text", "t":
        textMode = true

    of cmdEnd: assert(false)

  delay = max(delay, 1) * 1000
  if textMode:
    runTextMode(delay)
  else:
    runGraphMode(delay, size)


when isMainModule:
  main()
