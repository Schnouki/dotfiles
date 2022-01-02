import chroma

type
  ColorType* = enum
    CpuColors, MemColors, SwapColors


# ColorHCL = ColorPolarLUV
#   h: hue angle, 0.0 to 360.0
#   c: chroma
#   l: luminance

const
  CPU_COLORS_DEF = @[
    (0.1, "3f5f3f".parseHex.polarLUV),  # zb-green-4
    (0.5, "b09f6f".parseHex.polarLUV),  # zb-yellow-4
    (0.9, "8c5353".parseHex.polarLUV),  # zb-red-4
  ]
  MEM_COLORS_DEF = @[
    (0.5, "4c7073".parseHex.polarLUV),  # zb-blue-4
    (0.8, "9f6f4f".parseHex.polarLUV),  # zb-orange-4
    (0.9, "8c5353".parseHex.polarLUV),  # zb-red-4
  ]
  SWAP_COLORS_DEF = @[
    (0.5, "4c7073".parseHex.polarLUV),  # zb-blue-4
    (0.8, "9f6f4f".parseHex.polarLUV),  # zb-orange-4
    (0.9, "8c5353".parseHex.polarLUV),  # zb-red-4
  ]


proc interpolate(c1, c2: ColorPolarLUV, a: float): ColorPolarLUV =
  if a <= 0.0:
    return c1
  if a >= 1.0:
    return c2

  # From https://github.com/gka/chroma.js/blob/master/src/interpolator/_hsx.js
  let
    dh =
      if c2.h > c1.h and c2.h - c1.h > 180.0:
        c2.h - (c1.h + 360.0)
      elif c2.h < c1.h and c1.h - c2.h > 180.0:
        c2.h + 360.0 - c1.h
      else:
        c2.h - c1.h

  result.h = c1.h + a * dh
  result.c = c1.c + a * (c2.c - c1.c)
  result.l = c1.l + a * (c2.l - c1.l)


proc findColor(colors: seq[(float, ColorPolarLUV)], v: float): ColorPolarLUV =
  var pv = -1.0
  var pc = colors[0][1]

  for nv, nc in colors.items:
    if pv < 0 and v <= nv:
      return nc
    if pv <= v and v <= nv:
      let a = (v - pv) / (nv - pv)
      return interpolate(pc, nc, a)

    pv = nv
    pc = nc

  return pc


proc getBarColor*(t: ColorType, v: float): string =
  let colors = case t
               of CpuColors: CPU_COLORS_DEF
               of MemColors: MEM_COLORS_DEF
               of SwapColors: SWAP_COLORS_DEF
  return findColor(colors, v).color().toHtmlHex()
