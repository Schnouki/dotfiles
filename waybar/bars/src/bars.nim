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
  var cst1 = getCpuStats()

  while true:
    sleep(delay)
    let cst2 = getCpuStats()
    var line = "CPU: "
    for cpu, cst in cst2.pairs:
      let usage = usagePct(cst1[cpu], cst) * 100.0
      line &= &"{usage:5.1f}% "
    cst1 = cst2

    let mst = getMemStat()
    let memUsage = mst.memUsagePct * 100.0
    let memUnavail = mst.memUnavailPct * 100.0
    let swapUsage = mst.swapUsagePct * 100.0
    line &= &"\nMem: {memUsage:.1f}% (unavailable: {memUnavail:.1f}%)  Swap: {swapUsage:.1f}%"

    echo line


proc getRune(t: ColorType, v: float): string =
  let idx = (v * len(RUNES).float).int
  let r = RUNES[idx.clamp(0, len(RUNES)-1)]
  let col = getBarColor(t, v)
  return &"<span color=\"{col}\">{r}</span>"


proc updateBar(b: seq[string], t: ColorType, v: float): seq[string] =
  return b[1..^1] & getRune(t, v)


proc asBar(d: seq[string]): string =
  return "⎹<span font_desc=\"Bars\">" & d.join() & "</span>⎸"


proc runGraphMode(delay, size: int) =
  var cst1 = getCpuStats()

  let cpuBars = newOrderedTable[string, seq[string]]()
  for cpu in cst1.keys:
    cpuBars[cpu] = sequtils.repeat("\u3000", size)

  var memUsageBar = sequtils.repeat("\u3000", size)
  var memUnavailBar = sequtils.repeat("\u3000", size)
  var swapUsageBar = sequtils.repeat("\u3000", size)

  sleep(500)
  while true:
    var tooltip = newSeq[string]()

    # CPU
    let cst2 = getCpuStats()
    for cpu, cst in cst2.pairs:
      let usage = usagePct(cst1[cpu], cst)
      let bar = cpuBars[cpu].updateBar(CpuColors, usage)
      cpuBars[cpu] = bar
      tooltip.add(&"{cpu:5}: {usage * 100.0:5.1f}% " & bar.asBar())
    cst1 = cst2

    # Memory
    let mst = getMemStat()
    let memUsage = mst.memUsagePct
    let memUnavail = mst.memUnavailPct
    let swapUsage = mst.swapUsagePct
    memUsageBar = memUsageBar.updateBar(MemColors, memUsage)
    memUnavailBar = memUnavailBar.updateBar(MemColors, memUnavail)
    swapUsageBar = swapUsageBar.updateBar(SwapColors, swapUsage)
    tooltip.add("")
    tooltip.add(&"Mem (free):  {memUsage * 100.0:5.1f}% " & memUsageBar.asBar())
    tooltip.add(&"Mem (avail): {memUnavail * 100.0:5.1f}% " & memUnavailBar.asBar())
    tooltip.add(&"Swap:        {swapUsage * 100.0:5.1f}% " & swapUsageBar.asBar())

    # Output
    let text = "" & cpuBars["cpu"].asBar() & "" & memUnavailBar.asBar()
    echo(%* {"text": text, "tooltip": "<tt>" & tooltip.join("\n") & "</tt>"})

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
