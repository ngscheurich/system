-- Custom palette for KANAGAWA.nvim by Tommaso Laurenzi
-- https://github.com/rebelot/kanagawa.nvim
--
-- ## Changes
--
-- The base ("wave") colors are left mostly unmodified, with a shift away from
-- the yellow-ish foreground colors.
--
-- The "dragon" colors are modified to more closely resemble Tomorrow Night by
-- Chris Kempson (https://github.com/chriskempson/tomorrow-theme).
--
-- The "lotus" colors are modified to more closely resemeble Modus Operandi
-- Tinted by Protesilaos Stavrou (https://protesilaos.com/emacs/modus-themes).
--
-- stylua: ignore
return {
  fujiWhite     = "#cfcad2", -- Default foreground
  oldWhite      = "#b7adb1", -- Dark foreground (statuslines)
  fujiGray      = "#727169", -- Comments

  sumiInk0      = "#16161D", -- Dark background (statuslines and floating windows)
  sumiInk1      = "#181820", -- Dark background
  sumiInk2      = "#1a1a22", -- Dark background
  sumiInk3      = "#1F1F28", -- Default background
  sumiInk4      = "#2A2A37", -- Signcolumn
  sumiInk5      = "#252531", -- Cursorline
  sumiInk6      = "#54546D", -- Whitespace

  waveBlue1     = "#223249", -- Popup background, visual selection background
  waveBlue2     = "#2D4F67", -- Popup selection background, search background

  winterGreen   = "#2B3328", -- Diff Add (background)
  winterYellow  = "#49443C", -- Diff Change (background)
  winterRed     = "#43242B", -- Diff Deleted (background)
  winterBlue    = "#252535", -- Diff Line (background)
  autumnGreen   = "#76946A", -- Git Add
  autumnRed     = "#C34043", -- Git Delete
  autumnYellow  = "#DCA561", -- Git Change

  samuraiRed    = "#E82424", -- Diagnostic Error
  roninYellow   = "#FF9E3B", -- Diagnostic Warning
  waveAqua1     = "#6A9589", -- Diagnostic Info
  dragonBlue    = "#658594", -- Diagnostic Hint

  springViolet1 = "#938AA9", -- Light foreground

  oniViolet     = "#957FB8", -- Statements and Keywords
  crystalBlue   = "#7E9CD8", -- Functions and Titles
  springViolet2 = "#9CABCA", -- Brackets and punctuation
  springBlue    = "#7FB4CA", -- Specials and builtin functions
  lightBlue     = "#A3D4D5", -- Not used
  waveAqua2     = "#7AA89F", -- Types
  springGreen   = "#98BB6C", -- Strings
  boatYellow1   = "#938056", -- Not used
  boatYellow2   = "#C0A36E", -- Operators, RegEx
  carpYellow    = "#E6C384", -- Identifiers
  sakuraPink    = "#D27E99", -- Numbers
  waveRed       = "#E46876", -- Standout specials 1 (builtin variables)
  peachRed      = "#FF5D62", -- Standout specials 2 (exception handling, return)
  surimiOrange  = "#FFA066", -- Constants, imports, booleans

  katanaGray   = "#717C7C", -- Deprecated

  dragonBlack0  = "#0c0c0d",
  dragonBlack1  = "#111314",
  dragonBlack2  = "#262627",
  dragonBlack3  = "#1d1f21",
  dragonBlack4  = "#20222a",
  dragonBlack5  = "#24272e",
  dragonBlack6  = "#616264",
  dragonWhite   = "#c5c8c6",
  dragonGreen   = "#87a987",
  dragonGreen2  = "#789978",
  dragonPink    = "#a292a3",
  dragonOrange  = "#ce8a5b",
  dragonOrange2 = "#bb7b50",
  dragonGray    = "#a6a69c",
  dragonGray2   = "#9e9b93",
  dragonGray3   = "#7a8382",
  dragonBlue2   = "#7090ab",
  dragonViolet  = "#997ea2",
  dragonRed     = "#e46876",
  dragonAqua    = "#7aa89f",
  dragonAsh     = "#676b76",
  dragonTeal    = "#949fb5",
  dragonYellow  = "#e1b683",

  lotusInk1    = "#3f3f4b",
  lotusInk2    = "#193668",
  lotusGray    = "#9b7e6d",
  lotusGray2   = "#8a7f77",
  lotusGray3   = "#9f9690",
  lotusWhite0  = "#c9b9b0",
  lotusWhite1  = "#dfd5cf",
  lotusWhite2  = "#efe9dd",
  lotusWhite3  = "#fbf7f0",
  lotusWhite4  = "#efe9dd",
  lotusWhite5  = "#f8efed",
  lotusViolet1 = "#a09cac",
  lotusViolet2 = "#766b90",
  lotusViolet3 = "#c9cbd1",
  lotusViolet4 = "#624c83",
  lotusBlue1   = "#c7d7e0",
  lotusBlue2   = "#b5cbd2",
  lotusBlue3   = "#9fb5c9",
  lotusBlue4   = "#4d699b",
  lotusBlue5   = "#5d57a3",
  lotusGreen   = "#6f894e",
  lotusGreen2  = "#6e915f",
  lotusGreen3  = "#b7d0ae",
  lotusPink    = "#b35b79",
  lotusOrange  = "#cc6d00",
  lotusOrange2 = "#e98a00",
  lotusYellow  = "#77713f",
  lotusYellow2 = "#836f4a",
  lotusYellow3 = "#de9800",
  lotusYellow4 = "#f9d791",
  lotusRed     = "#c84053",
  lotusRed2    = "#d7474b",
  lotusRed3    = "#e82424",
  lotusRed4    = "#d9a594",
  lotusAqua    = "#597b75",
  lotusAqua2   = "#5e857a",
  lotusTeal1   = "#4e8ca2",
  lotusTeal2   = "#6693bf",
  lotusTeal3   = "#5a7785",
  lotusCyan    = "#d7e3d8",
}
