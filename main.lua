--- A basic test of the ideas in [this paper](https://graphics.stanford.edu/courses/cs468-05-fall/Papers/p471-muller.pdf),
-- so far only implementing linear deformation.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

local CX, CY = display.contentCenterX, display.contentCenterY

--
--
--

local function UpdateCorner (path, xprop, yprop, dx, dy)
  if path then
    path[xprop] = path[xprop] + dx
    path[yprop] = path[yprop] + dy
  end
end

--
--
--

local function ReactToMove (node, dx, dy)
  UpdateCorner(node.p1, "x1", "y1", dx, dy)
  UpdateCorner(node.p2, "x2", "y2", dx, dy)
  UpdateCorner(node.p3, "x3", "y3", dx, dy)
  UpdateCorner(node.p4, "x4", "y4", dx, dy)
end

--
--
--

local function NodeTouch (event)
  local node, phase = event.target, event.phase

  if phase == "began" then
    node.xoff, node.yoff = event.x - node.x, event.y - node.y

    display.getCurrentStage():setFocus(node)
  elseif phase ~= "moved" then
    node.xoff = nil

    display.getCurrentStage():setFocus(nil)
  elseif node.xoff then
    local x, y, old_x, old_y = event.x - node.xoff, event.y - node.yoff, node.x, node.y

    --
    --
    --

    local bounds = node.parent.bounds

    node.x = math.max(bounds.xMin, math.min(bounds.xMax, x))
    node.y = math.max(bounds.yMin, math.min(bounds.yMax, y))

    --
    --
    --

    ReactToMove(node, node.x - old_x, node.y - old_y)
  end
end

--
--
--

graphics.defineEffect{
  category = "filter", name = "texture",

  uniformData = {
    {
      name = "ul",
      type = "vec2",
      index = 0
    }, {
      name = "ll",
      type = "vec2",
      index = 1
    }, {
      name = "lr",
      type = "vec2",
      index = 2
    }, {
      name = "ur",
      type = "vec2",
      index = 3
    }
  },

  vertex = [[
    uniform P_POSITION vec2 u_UserData0; // upper-left
    uniform P_POSITION vec2 u_UserData1; // lower-left
    uniform P_POSITION vec2 u_UserData2; // lower-right
    uniform P_POSITION vec2 u_UserData3; // upper-right
  
    P_POSITION vec2 VertexKernel (P_POSITION vec2 p)
    {
      P_UV vec2 uv = CoronaTexCoord
    #ifdef TEX_COORD_Z
      / v_TexCoordZ
    #endif
      ;
    
      P_POSITION vec2 top = mix(u_UserData0, u_UserData3, uv.x);
      P_POSITION vec2 bot = mix(u_UserData1, u_UserData2, uv.x);
    
      v_TexCoord = mix(top, bot, uv.y);
    
    #ifdef TEX_COORD_Z
      v_TexCoordZ = 1.;
    #endif
    
      return p;
    }
  ]]
}

--
--
--

local UV = {}

local function SetCornerUV (obj, what, u, v)
  UV[1], UV[2] = u, v

  obj.fill.effect[what] = UV
end

--
--
--

local W, H = .75 * CX, .75 * CY

local LCX, LCY = .5 * CX, .5 * CY
local RCX, RCY = 1.5 * CX, LCY
local DCX, DCY = CX, 1.5 * CY

local LX1, LY1 = LCX - W / 2, LCY - H / 2
local RX1, RY1 = RCX - W / 2, RCY - H / 2
local DX1, DY1 = DCX - W / 2, DCY - H / 2

local NCols, NRows = 5, 5

--
--
--

local function Line (x1, y1, x2, y2)
  local line = display.newLine(math.round(x1), math.round(y1), math.round(x2), math.round(y2))
  
  line:setStrokeColor(.5, .5)
  line:toBack()
  
  line.strokeWidth = 2
end

--
--
--

local CellGroup = display.newGroup()

--
--
--

local LGroup, RGroup = display.newGroup(), display.newGroup()

--
--
--

local DummyGroup = display.newGroup()

DummyGroup.isVisible = false

--
--
--

local DummyGroup2 = display.newGroup()

DummyGroup2.isVisible = false

--
--
--

local DemoGroup = display.newGroup()

DemoGroup.isVisible = false

--
--
--

local DU = 1 / (NCols - 1)
local DV = 1 / (NRows - 1)
local DW, DH = DU * W, DV * H

--
--
--

--
--
--

local function AddCell (into, node, uoff, voff)
  local cell = display.newImageRect(into, "Image1.jpg", DW, DH)

  cell.anchorX, cell.x = 1, node.x
  cell.anchorY, cell.y = 1, node.y
  cell.alpha = .75

  --
  --
  --

  cell.fill.effect = "filter.custom.texture"

  SetCornerUV(cell, "ul", uoff - DU, voff - DV)
  SetCornerUV(cell, "ll", uoff - DU, voff)
  SetCornerUV(cell, "lr", uoff, voff)
  SetCornerUV(cell, "ur", uoff, voff - DV)

  --
  --
  --

  local ngroup, path = node.parent, cell.path

  node.p3 = path

  local n = ngroup.numChildren

  ngroup[n - NCols - 1].p1 = path
  ngroup[n - 1].p2 = path
  ngroup[n - NCols].p4 = path
end

--
--
--

for j = 1, NRows do
  local voff = (j - 1) * DV
  local yoff = (j - 1) * DH
  local y = LY1 + yoff
  local dummy_y = DY1 + yoff

  for i = 1, NCols do
    local uoff = (i - 1) * DU
    local xoff = (i - 1) * DW
    local lx = math.round(LX1 + xoff)
    local rx = math.round(RX1 + xoff)
    local dummy_x = math.round(DX1 + xoff)

    Line(lx, LY1, lx, LY1 + H)
    Line(rx, LY1, rx, LY1 + H)
  
    local lnode = display.newCircle(LGroup, lx, y, 10)
    local rnode = display.newCircle(RGroup, rx, y, 10)
    local dummy = display.newCircle(DummyGroup, dummy_x, dummy_y, 10)
    local dummy2 = display.newCircle(DummyGroup2, dummy_x, dummy_y, 10)
    
    rnode.old_x, rnode.old_y = rnode.x, rnode.y
    
    if i == 1 then
      Line(lx, y, lx + W, y)
      Line(rx, y, rx + W, y)

      if j == 1 then
        LGroup.bounds = { xMin = lx, yMin = y, xMax = lx + W, yMax = y + H }
        RGroup.bounds = { xMin = rx, yMin = y, xMax = rx + W, yMax = y + H }
      end
    elseif j ~= 1 then
      AddCell(CellGroup, lnode, uoff, voff)
      AddCell(CellGroup, rnode, uoff, voff)
      AddCell(DemoGroup, dummy, uoff, voff)
    end
    
    -- TODO: Aqq

    lnode:setFillColor(0, 1, 0)
    lnode:setStrokeColor(0, .5, 0)
    lnode:addEventListener("touch", NodeTouch)

    rnode:setFillColor(0, 0, 1)
    rnode:setStrokeColor(0, 0, .5)
    rnode:addEventListener("touch", NodeTouch)
    
    lnode.strokeWidth, rnode.strokeWidth = 2, 2
  end
end

--
--
--

display.newText("Original", LCX, LY1 - 40, native.systemFontBold, 16)
display.newText("Deformed", RCX, RY1 - 40, native.systemFontBold, 16)

--
--
--

local function Outer (out, px, py, qx, qy)
  out[1] = px * qx
  out[2] = px * qy
  out[3] = py * qx
  out[4] = py * qy
end

--
--
--

local function At_A (out, m)
  local a, b, c, d = unpack(m)
  local off_diag = a * b + c * d

  out[1] = a * a + c * c
  out[2] = off_diag
  out[3] = off_diag
  out[4] = b * b + d * d
end

--
--
--

local function Sqrt (out, m)
  local a, b, c, d = unpack(m)
  local trace = a + d

  if 1 + trace * trace ~= 1 then
    -- https://en.wikipedia.org/wiki/Square_root_of_a_2_by_2_matrix#A_general_formula
    local det = a * d - b * c
    local s = math.sqrt(det)
    local t = math.sqrt(trace + 2 * s)

    out[1] = (a + s) / t
    out[2] = b / t
    out[3] = c / t
    out[4] = (d + s) / t
  else
    out[1] = 0
    out[2] = 0
    out[3] = 0
    out[4] = 0
  end
end

--
--
--

local function Invert (out, m)
  local a, b, c, d = unpack(m)
  local det = a * d - b * c

  if 1 + det * det ~= 1 then
    out[1] = d / det
    out[2] = -b / det
    out[3] = -c / det
    out[4] = a / det
    
    return true
  end
end

--
--
--

local function MatrixMul (out, m1, m2)
  local a, b, c, d = unpack(m1)
  local e, f, g, h = unpack(m2)

  out[1] = a * e + b * g
  out[2] = a * f + b * h
  out[3] = c * e + d * g
  out[4] = c * f + d * h
end

--
--
--

local function MatrixTimesVector (m, x, y)
  return m[1] * x + m[2] * y, m[3] * x + m[4] * y
end

--
--
--

local Update

--
--
--

local N = RGroup.numChildren

--
--
--

local function CenterOfMass (group)
  local xcm, ycm = 0, 0

  for i = 1, N do
    local node = group[i]

    xcm, ycm = xcm + node.x, ycm + node.y -- n.b. assumes uniform density
  end
  
  return xcm / N, ycm / N
end

--
--
--

local function GetRelativeLocations (group)
  local xcm, ycm = CenterOfMass(group)

  for i = 1, N do
    local node = group[i]
    
    node.dx, node.dy = node.x - xcm, node.y - ycm
  end
end

--
--
--

local function DUMP (name, m)
do return end
  print("MATRIX:", name)
  print(("%.7g, %.7g"):format(m[1], m[2]))
  print(("%.7g, %.7g"):format(m[3], m[4]))
  print("")
end

local function SumOuters (out, lgroup, rgroup)
  local a, b, c, d = 0, 0, 0, 0

  for i = 1, N do
    local q, p = lgroup[i], rgroup[i]

    Outer(out, p.dx, p.dy, q.dx, q.dy)
DUMP("OUT"..i, out)
    a, b, c, d = a + out[1], b + out[2], c + out[3], d + out[4]
if false then
print(("inc = %i: %.7g, %.7g, %.7g, %.7g"):format(i, a, b, c, d))
print("")
end
  end

  out[1], out[2], out[3], out[4] = a, b, c, d
end

--
--
--

local Aqq, Temp = {}, {}

--
--
--

local GoGroup = display.newGroup()

local Go = display.newRect(GoGroup, CX, .5 * CY, 50, 150)

Go:addEventListener("touch", function(event)
  local button, phase = event.target, event.phase

  if phase == "began" then
    button.is_touched = true

    display.getCurrentStage():setFocus(button)
  elseif phase ~= "moved" and button.is_touched then
    button.is_touched = false
    button.parent.isVisible = false

    --
    --
    --

    GetRelativeLocations(LGroup) -- one-time, if not doing plasticity
    SumOuters(Temp, LGroup, LGroup) -- ditto
    Invert(Aqq, Temp)

    for i = 1, N do
      local p, d = RGroup[i], DummyGroup[i]
      local dx, dy = p.x - p.old_x, p.y - p.old_y

      ReactToMove(d, dx, dy)

      d.x, d.vx = d.x + dx, 0
      d.y, d.vy = d.y + dy, 0
    end

    --
    --
    --
    
    DemoGroup.isVisible = true

    --
    --
    --

    Runtime:addEventListener("enterFrame", Update)

    display.getCurrentStage():setFocus(nil)
  end

  return true
end)

Go:setFillColor(.7, .3, .2)
Go:setStrokeColor(.2)

Go.strokeWidth = 3

display.newText(GoGroup, "Go", Go.x, Go.y, native.systemFontBold, 24)

--
--
--

local Alpha = .575

--
--
--

local Beta = .3--.9

--
--
--

local Last

--
--
--

local A, Apq, Ap, S, Inv, R, Lin = {}, {}, {}, {}, {}, {}, {}

--
--
--

local function RescaleMatrix (m)
  local a, b, c, d = unpack(m)
  local det = a * d - b * c
  local d2 = math.sqrt(det)

  m[1] = a / d2
  m[2] = b / d2
  m[3] = c / d2
  m[4] = d / d2
end

--
--
--

local function Lerp (out, m1, m2, t)
  local s = 1 - t

  out[1] = m1[1] * s + m2[1] * t
  out[2] = m1[2] * s + m2[2] * t
  out[3] = m1[3] * s + m2[3] * t
  out[4] = m1[4] * s + m2[4] * t
end

--
--
--

local WithIntegration = true--false

--
--
--

function Update (event)
  local dt, now = 0, event.time

  if Last then
    dt = (now - Last) / 1000
  end

  Last = now

  --
  --
  --

  GetRelativeLocations(DummyGroup)

  --
  --
  --

  SumOuters(Apq, LGroup, DummyGroup)
  At_A(Ap, Apq)
DUMP("AtA", Ap)
  Sqrt(S, Ap)
DUMP("S", S)
  if dt > 0 and Invert(Inv, S) then
DUMP("INV", Inv)
    MatrixMul(R, Apq, Inv)
DUMP("R", R)
    MatrixMul(A, Apq, Aqq)
DUMP("A", A)
    RescaleMatrix(A)
DUMP("ResA", A)
    Lerp(Lin, R, A, Beta)
DUMP("Lin", Lin)
    local scale = Alpha / dt

if WithIntegration then
  -- cf. https://github.com/danielflower/MeshlessDeformations/blob/master/MeshlessDeformations/DeformableObject.cpp#L127
  -- also section 3.1 of the accompanying "MeshlessDeformationsReport.pdf"
  for i = 1, N do
    local d, d2 = DummyGroup[i], DummyGroup2[i]
    local vx, vy = d.vx, d.vy -- TODO add any forces...
    
    d2.x, d2.y = d.x + vx * dt, d.y + vy * dt
  end
  
  GetRelativeLocations(DummyGroup2)
end

    for i = 1, N do
      local q, d = LGroup[i], DummyGroup[i]
      local gx, gy = MatrixTimesVector(Lin--[[R]], q.dx, q.dy)
if WithIntegration then
      local d2 = DummyGroup2[i]
      local vx, vy = d.vx + scale * (gx - d2.dx), d.vy + scale * (gy - d2.dy)
      local dx, dy = dt * vx, dt * vy

      d.x, d.vx = d.x + dx, vx
      d.y, d.vy = d.y + dy, vy

      ReactToMove(d, dx, dy)
else
gx,gy=gx-d.dx,gy-d.dy
  local len = math.sqrt(gx*gx+gy*gy)
  if len > 40 then
    gx, gy = gx * 40 / len, gy * 40 / len
  end
  local dx, dy = dt*gx,dt*gy
  d.x = d.x + dx
  d.y = d.y + dy

  ReactToMove(d, dx, dy)
end
    end
  else
    -- ????
  end
end

-- xi, vi, R, cm, masses

-- go!
-- alpha, beta sliders