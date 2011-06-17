# Author::  Vikas Kanhaua

# == Contrib Import {{{
#

begin
  require "#{ENV["HOME"]}/.config/subtle/subtle-contrib/ruby/selector.rb"
  require "#{ENV["HOME"]}/.config/subtle/subtle-contrib/ruby/merger.rb"

  Subtle::Contrib::Selector.font  = "xft:MonteCarlo"
  Subtle::Contrib::Merger.font    = "xft:MonteCarlo"
rescue LoadError
end

# }}}

# == Options {{{
#

set :step     , 5
set :snap     , 10
set :gravity  , :center_1
set :urgent   , true
set :resize   , false

set :font     , "xft:MonteCarlo"

set :separator, "|"

set :wmname   , "LG3D"

# }}}

# == Screen {{{
#

screen 1 do
  top [ :title, :spacer, :views ]
end

# }}}

# == Colors {{{
#

style :title do
  foreground     "#cdad00"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :focus do
  foreground     "#cdad00"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :urgent do
  foreground     "#cd5c5c"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :occupied do
  foreground     "#659fbd"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :views do
  foreground     "#757575"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :sublets do
  foreground     "#fea63c"
  background     "#1a1a1a"
  padding        0, 3, 0, 3
end

style :separator do
  background     "#1a1a1a"
  foreground     "#757575"
end

style :clients do
  active         "#303030", 1
  inactive       "#202020", 1
end

style :subtle do
  panel          "#1a1a1a"
  stipple        "#757575"
end

# }}}

# == Gravities {{{
#

gravity :top_left,       [  0,  0,  50,  50 ]
gravity :top,            [  0,  0, 100,  50 ]
gravity :top_right,      [ 50,  0,  50,  50 ]
gravity :left,           [  0,  0,  50, 100 ]
gravity :center,         [  0,  0, 100, 100 ]
gravity :center_1,       [  2,  2,  96,  80 ]
gravity :right,          [ 50,  0,  50, 100 ]
gravity :bottom_left,    [  0, 50,  50,  50 ]
gravity :bottom_left33,  [  0, 50,  50,  30 ]
gravity :bottom,         [  0, 50, 100,  50 ]
gravity :bottom_right,   [ 50, 50,  50,  50 ]
gravity :bottom_right33, [ 50, 50,  50,  30 ]

# Gimp
gravity :gimp_image,     [ 10,  0,  80, 100 ]
gravity :gimp_toolbox,   [  0,  0,  10, 100 ]
gravity :gimp_dock,      [ 90,  0,  10, 100 ]

# }}}

# == Grabs {{{
#

# Main {{{
#
# Jump to view1, view2, ...
grab "W-S-1",       :ViewJump1
grab "W-S-2",       :ViewJump2
grab "W-S-3",       :ViewJump3
grab "W-S-4",       :ViewJump4
grab "W-S-5",       :ViewJump5

# Switch current view
grab "W-1",         :ViewSwitch1
grab "W-2",         :ViewSwitch2
grab "W-3",         :ViewSwitch3
grab "W-4",         :ViewSwitch4
grab "W-5",         :ViewSwitch5

# Select next and prev view */
grab "KP_Add",      :ViewNext
grab "KP_Subtract", :ViewPrev

grab "W-q",         :SubtleReload
grab "W-C-r",       :SubtleRestart
grab "W-S-q",       :SubtleQuit

grab "W-B1",        :WindowMove
grab "W-B3",        :WindowResize

grab "W-t",         :WindowFloat
grab "W-space",     :WindowFull
grab "W-s",         :WindowStick
grab "W-r",         :WindowRaise
grab "W-n",         :WindowLower

grab "W-h", :WindowLeft
grab "W-j", :WindowDown
grab "W-k", :WindowUp
grab "W-l", :WindowRight

grab "W-S-c",       :WindowKill

# Cycle between given gravities
grab "W-KP_7",      [ :top_left                      ]
grab "W-KP_8",      [ :top                           ]
grab "W-KP_9",      [ :top_right                     ]
grab "W-KP_4",      [ :left                          ]
grab "W-KP_5",      [ :center        ,     :center_1 ]
grab "W-KP_6",      [ :right                         ]
grab "W-KP_1",      [ :bottom_left33 ,  :bottom_left ]
grab "W-KP_2",      [ :bottom                        ]
grab "W-KP_3",      [ :bottom_right33, :bottom_right ]

# }}}

# Contrib {{{
#
grab "W-m" do
  Subtle::Contrib::Merger.run
end

grab "W-g" do
  Subtle::Contrib::Selector.run
end

# Scratchpad
grab "F12" do
  if((c = Subtlext::Client["scratch"]))
    c.toggle_stick
    c.focus
  elsif((c = Subtlext::Subtle.spawn("urxvtc -name scratch")))
    c.tags  = [ ]
    c.flags = [ :stick ]
  end
end

# }}}

# User Programs {{{
#
grab "W-Return"            , "urxvtc"
grab "XF86Sleep"           , "alock -auth md5:file=/home/vikki/docs/passphrase"
grab "XF86Search"          , "firefox"
grab "XF86AudioRaiseVolume", "amixer -q set 'Front' 2+"
grab "XF86AudioLowerVolume", "amixer -q set 'Front' 2-"
grab "XF86AudioMute"       , "amixer -q set 'Front' toggle"
grab "XF86Mail"            , "mpc -q toggle"
grab "W-Down"              , "mpc -q stop"
grab "W-Right"             , "mpc -q next"
grab "W-Left"              , "mpc -q prev"
grab "W-a"                 , "zathura"
grab "W-b"                 , "favsong -b"
grab "W-c"                 , "chromium"
grab "W-e"                 , "eject -T /dev/sr0"
grab "W-f"                 , "favsong"
grab "W-o"                 , "libreoffice"
grab "W-p"                 , "dmenu_run -fn '-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*' -nb '#1a1a1a' -nf '#cdad00' -sb '#1a1a1a' -sf '#f1f1f1'"
grab "W-w"                 , "ranwall"
grab "W-x"                 , "xterm"
grab "W-XF86Mail"          , "echo pause > ~/.mplayer/mplayer_fifo"
grab "W-Home"              , "sudo shutdown -r now"
grab "W-End"               , "sudo shutdown -h now"

# Using 5.1 channels is quite a pain
grab "W-XF86AudioRaiseVolume", "amixer -q set 'Surround' 2+"
grab "W-XF86AudioLowerVolume", "amixer -q set 'Surround' 2-"
grab "W-XF86AudioMute"       , "amixer -q set 'Surround' toggle"

# }}}

# }}}

# == Tags {{{
#

tag "terms" do
  match   "xterm|[u]?rxvt"
  gravity :center_1
end

tag "browser" do
  match   "uzbl|opera|firefox|navigator|chromium"
  gravity :center_1
end

tag "media" do
  match   "mplayer|vlc"
  float   true
  stick   true
end

tag "office" do
  match   "libreoffice"
end

# Gimp
tag "gimp_image" do
  match   :role => "gimp-image-window"
  gravity :gimp_image
end

tag "gimp_toolbox" do
  match   :role => "gimp-toolbox$"
  gravity :gimp_toolbox
end

tag "gimp_dock" do
  match   :role => "gimp-dock"
  gravity :gimp_dock
end

tag "gimp" do
  match   :class => "gimp"
end

# }}}

# == Views {{{
#
iconpath = "#{ENV["HOME"]}/.config/subtle/icons"

view "term" do
  match    "terms|default"
  icon     "#{iconpath}/terminal.xbm"
end

view "web" do
  match    "browser"
  icon     "#{iconpath}/world.xbm"
end

view "media" do
  match    "media"
  icon     "#{iconpath}/quote.xbm"
end

view "work" do
  match    "gimp*|office"
  icon     "#{iconpath}/pencil.xbm"
end

view "else" do
  icon     "#{iconpath}/bug.xbm"
end

# }}}

# == Hooks {{{
#

on :start do
  system("devmon -g --exec-on-drive 'notify %f mounted on %d' &")
end

on :exit do
  system("kill `pgrep devmon`")
end

# }}}
