import std/[ strutils, tables ]

type
  CpuStat* = ref object
    user*: uint
    nice*: uint
    system*: uint
    idle*: uint
    iowait*: uint
    irq*: uint
    softirq*: uint
    steal*: uint
    guest*: uint
    guest_nice*: uint

  CpuStats* = OrderedTableRef[string, CpuStat]

  MemStat* = ref object
    memTotal*: uint
    memFree*: uint
    memAvail*: uint
    swapTotal*: uint
    swapFree*: uint


proc getCpuStats*(): CpuStats =
  result = newOrderedTable[string, CpuStat]()
  for line in "/proc/stat".lines:
    let words = line.splitWhitespace()
    if not words[0].startsWith("cpu"):
      break
    result[words[0]] = CpuStat(
      user: words[1].parseUInt(),
      nice: words[2].parseUInt(),
      system: words[3].parseUInt(),
      idle: words[4].parseUInt(),
      iowait: words[5].parseUInt(),
      irq: words[6].parseUInt(),
      softirq: words[7].parseUInt(),
      steal: words[8].parseUInt(),
      guest: words[9].parseUInt(),
      guest_nice: words[10].parseUInt(),
    )


proc usagePct*(a, b: CpuStat): float =
  let total = (
    (b.user - a.user) +
    (b.nice - a.nice) +
    (b.system - a.system) +
    (b.idle - a.idle) +
    (b.iowait - a.iowait) +
    (b.irq - a.irq) +
    (b.softirq - a.softirq) +
    (b.steal - a.steal) +
    (b.guest - a.guest) +
    (b.guest_nice - a.guest_nice)
  )
  let idle = (b.idle - a.idle)
  return (total - idle).float / total.float


proc getMemStat*(): MemStat =
  result = MemStat()
  for line in "/proc/meminfo".lines:
    let words = line.splitWhitespace()
    case words[0]
    of "MemTotal:":
      result.memTotal = words[1].parseUInt()
    of "MemFree:":
      result.memFree = words[1].parseUInt()
    of "MemAvailable:":
      result.memAvail = words[1].parseUInt()
    of "SwapTotal:":
      result.swapTotal = words[1].parseUInt()
    of "SwapFree:":
      result.swapFree = words[1].parseUInt()


proc memUsagePct*(m: MemStat): float =
  return (m.memTotal - m.memFree).float / m.memTotal.float


proc memUnavailPct*(m: MemStat): float =
  return (m.memTotal - m.memAvail).float / m.memTotal.float


proc swapUsagePct*(m: MemStat): float =
  return (m.swapTotal - m.swapFree).float / m.swapTotal.float
