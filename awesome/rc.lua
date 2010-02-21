-- {{{ libraries
require("awful")
require("awful.autofocus")
require("awful.rules")
require("beautiful")
require("naughty")
require("vicious")
require("teardrop")
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/zenburn.lua")

-- This is used later as the default terminal and editor to run.
editor = os.getenv("EDITOR") or "vim"
terminal = "urxvtc"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
  awful.layout.suit.floating,
  awful.layout.suit.tile,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier
}
-- }}}   

-- {{{ naughty configuration
naughty.config.presets.normal.timeout          = 8
naughty.config.presets.normal.font             = beautiful.font or "Verdana 8"
naughty.config.presets.normal.icon_size        = 16
naughty.config.presets.normal.ontop            = true
naughty.config.presets.normal.fg               = beautiful.fg_focus or '#fea63c'
naughty.config.presets.normal.bg               = beautiful.bg_focus or '#535d6c'
naughty.config.presets.normal.border_color     = beautiful.border_focus or '#535d6c'
naughty.config.presets.normal.border_width     = 1
-- }}}

-- {{{ Tags
-- Define a tag table which will hold all screen tags.
tags = {
  names  = { "main", "web", "term", "media", "code" },
  layout = { layouts[3], layouts[7], layouts[5], layouts[1], layouts[2]
}}

for s = 1, screen.count() do
  -- Each screen has its own tag table.
  tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Wibox

-- {{{ defaults
spacer    = widget({ type = "textbox"  })
separator = widget({ type = "imagebox" })
spacer.text     = " "
separator.image = image(beautiful.widget_sep)
-- }}}

-- {{{ top

-- {{{ creation of widgets 

-- {{{ File system usage  
fsicon = widget({ type = "imagebox" })
fsicon.image = image(beautiful.widget_fs)
-- Initialize widgets
fs = {
  r = awful.widget.progressbar(),  h = awful.widget.progressbar(),
  s = awful.widget.progressbar(),  t = awful.widget.progressbar()
}
-- Progressbar properties
for _, w in pairs(fs) do
  w:set_width(6)
  w:set_height(12)
  w:set_vertical(true)
  w:set_background_color(beautiful.fg_off_widget)
  w:set_border_color(beautiful.border_widget)
  w:set_color(beautiful.fg_widget)
  w:set_gradient_colors({ beautiful.fg_widget,
     beautiful.fg_center_widget, beautiful.fg_end_widget
  }) 
end
-- caching
vicious.enable_caching(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",       599)
vicious.register(fs.h, vicious.widgets.fs, "${/home used_p}",   599)
vicious.register(fs.s, vicious.widgets.fs, "${/stuff used_p}",  599)
vicious.register(fs.t, vicious.widgets.fs, "${/mnt/tv used_p}", 599)
-- }}} 

-- {{{ Network usage
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image(beautiful.widget_net)
upicon.image = image(beautiful.widget_netup)
-- Initialize widget
netwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(netwidget, vicious.widgets.net, '<span color="'
  .. beautiful.fg_netdn_widget ..'">${ppp0 down_kb}</span> <span color="'
  .. beautiful.fg_netup_widget ..'">${ppp0 up_kb}</span>', 3)
-- }}}

-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
-- Progressbar properties
volbar:set_width(10)
volbar:set_height(12)
volbar:set_vertical(true)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_border_color(beautiful.border_widget)
volbar:set_color(beautiful.fg_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Enable caching
vicious.enable_caching(vicious.widgets.volume)
-- Register widgets
vicious.register(volbar,    vicious.widgets.volume, "$1",  2, "Master")
vicious.register(volwidget, vicious.widgets.volume, "$1%", 2, "Master")
-- }}}

-- {{{ Date and time
dateicon = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
-- Initialize widget
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, '<span color="#d6d6d6">%a %d %b</span>, %H:%M' , 61)
-- }}}

--{{{ mail
mailicon = widget({ type = "imagebox" })
mailicon.image = image(beautiful.widget_mail)
mailwidget = widget({ type = 'textbox' })
vicious.register(mailwidget, vicious.widgets.mdir, "$1", 113, {"/home/vikki/Mail/INBOX"})
--}}}

-- {{{ systray
mysystray = widget({ type = "systray" })
-- }}}

-- {{{ Create a wibox for each screen and add it
top_wibox = {}
mylayoutbox = {}
mytaglist = {}
mytasklist = {}
-- }}}

--  }}}

-- {{{  add each widget
for s = 1, screen.count() do
  -- Create an imagebox widget which will contains an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  mylayoutbox[s] = awful.widget.layoutbox(s)

  -- Create a taglist widget
  mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all)

  -- Create a tasklist widget
  mytasklist[s] = awful.widget.tasklist(function(c) return awful.widget.tasklist.label.focused(c, s) end)

  -- Create the wibox
  top_wibox[s] = awful.wibox({ 
    screen = s,
    fg = beautiful.fg_normal, height = 12, 
    bg = beautiful.bg_normal,
    border_color = beautiful.border_focus,
    border_width = beautiful.border_width,
    position = "top"
  })

  -- Add widgets to the wibox - order matters
  top_wibox[s].widgets = {
    {
      spacer, mytaglist[s],
      separator, spacer, mylayoutbox[s],
      separator, spacer,
      layout = awful.widget.layout.horizontal.leftright
    },
    spacer, datewidget, spacer, dateicon,
    separator, volwidget, spacer, volbar.widget, volicon, 
    separator, fs.t.widget, fs.s.widget, fs.h.widget, fs.r.widget, spacer, fsicon,
    separator, mailwidget, spacer, mailicon,
    separator, upicon, netwidget, dnicon, 
    separator, s == 1 and mysystray or nil,
    mytasklist[s],
    layout = awful.widget.layout.horizontal.rightleft
  }
end
-- }}}

-- }}} 

-- {{{ bottom

-- {{{ creation of widgets

-- hddtemp {{{
-- initialize
hdd = {
  sda = widget({ type = "textbox" }),
  sdb = widget({ type = "textbox" })
}
-- register
vicious.register(hdd.sda, vicious.widgets.hddtemp, 'sda <span color="'.. beautiful.fg_center_widget ..'">${/dev/sda}°C</span>', 53)
vicious.register(hdd.sdb, vicious.widgets.hddtemp, 'sdb <span color="'.. beautiful.fg_center_widget ..'">${/dev/sdb}°C</span>', 59)
-- }}}

-- mpd {{{
mpdwidget = widget({ type = 'textbox' })
-- register & custom o/p fn
vicious.register(mpdwidget, vicious.widgets.mpd,
  function (widget, args)
    if   args[1] == 'Stopped' then
      return '<span color="brown">mpd stopped</span>'
    else
      return '<span color="#fea63c">'.. args[1] ..'</span>'
    end
  end)
-- }}}

-- {{{ uptime
uptimewidget = widget({ type = 'textbox' })
uptimewidget.align = 'right'
vicious.register(uptimewidget, vicious.widgets.uptime,
  function (widget, args)
    return string.format('uptime<span color="#66aabb"> %2dd %02d:%02d</span>', args[1], args[2], args[3])
  end, 61)
-- }}} 

-- {{{ pkg updates
updatewidget = widget({ type = 'textbox' })
vicious.register(updatewidget, vicious.widgets.pkg,
  function (widget, args)
    if args[1] <= 50 then
      return '<span color="#88a175">'..args[1]..' pkg(s)</span> to upgrade'
    else
      return '<span color="red">'..args[1]..' pkg(s)</span> to upgrade'
    end
  end, 3600, 'Arch')
-- }}}

-- }}}

-- wibox created
bottom_wibox = {}
mypromptbox = {}

-- {{{ add each widget
for s = 1, screen.count() do
  -- Create a promptbox for each screen
  mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })

  -- Create the wibox
  bottom_wibox[s] = awful.wibox({ screen = s,
             fg = beautiful.fg_normal, 
             bg = beautiful.bg_normal, 
             border_color = beautiful.border_focus,
             border_width = beautiful.border_width,
             height = 12,
             position = "bottom"
           })

  -- Add widgets to the wibox - order matters
  bottom_wibox[s].widgets = {
    {
      spacer, updatewidget, spacer,
      separator, spacer, hdd.sda, spacer, 
      separator, spacer, hdd.sdb, spacer, separator,
      spacer, mypromptbox[s],
      layout = awful.widget.layout.horizontal.leftright
    },
    spacer, uptimewidget, spacer,
    separator,spacer, mpdwidget, spacer, separator,
    layout = awful.widget.layout.horizontal.rightleft
  }
end
-- }}}

-- }}}

-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
  awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

  -- Drop-down terminal
  awful.key({ modkey,           }, "s",      function () teardrop(terminal, "bottom", "right", 700, .40) end),

  awful.key({ modkey,           }, "Tab",    function () awful.client.focus.byidx( 1) if client.focus then client.focus:raise() end end),
  awful.key({ modkey, "Shift"   }, "Tab",    function () awful.client.focus.byidx(-1) if client.focus then client.focus:raise() end end),
  awful.key({ modkey,           }, "j",      function () awful.client.focus.history.previous() if client.focus then client.focus:raise() end end),

  -- Layout manipulation
  awful.key({ modkey, "Shift"   }, "j",      function () awful.client.swap.byidx(  1)    end),
  awful.key({ modkey, "Shift"   }, "k",      function () awful.client.swap.byidx( -1)    end),
  awful.key({ modkey, "Control" }, "j",      function () awful.screen.focus_relative( 1) end),
  awful.key({ modkey, "Control" }, "k",      function () awful.screen.focus_relative(-1) end),

  awful.key({ modkey,           }, "u",      awful.client.urgent.jumpto),

  -- Standard program
  awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
  awful.key({ modkey, "Control" }, "r",      awesome.restart),
  awful.key({ modkey, "Shift"   }, "q",      awesome.quit),

  awful.key({ modkey,           }, "l",      function () awful.tag.incmwfact( 0.05)    end),
  awful.key({ modkey,           }, "h",      function () awful.tag.incmwfact(-0.05)    end),
  awful.key({ modkey, "Shift"   }, "h",      function () awful.tag.incnmaster( 1)      end),
  awful.key({ modkey, "Shift"   }, "l",      function () awful.tag.incnmaster(-1)      end),
  awful.key({ modkey, "Control" }, "h",      function () awful.tag.incncol( 1)         end),
  awful.key({ modkey, "Control" }, "l",      function () awful.tag.incncol(-1)         end),
  awful.key({ modkey,           }, "space",  function () awful.layout.inc(layouts,  1) end),
  awful.key({ modkey, "Shift"   }, "space",  function () awful.layout.inc(layouts, -1) end),

  -- Prompt
  awful.key({ modkey            }, "p",      function () mypromptbox[mouse.screen]:run() end),
  awful.key({ modkey            }, "x",      function ()
                                               awful.prompt.run({ prompt = "Run Lua code: " },
                                               mypromptbox[mouse.screen].widget,
                                               awful.util.eval, nil,
                                               awful.util.getdir("cache") .. "/history_eval")
                                             end),

  -- custom apps keys
  awful.key({                   }, "XF86Search",           function () awful.util.spawn("firefox", false) end),
  awful.key({                   }, "XF86Sleep",            function () awful.util.spawn("slock", false) end),
  awful.key({                   }, "XF86AudioMute",        function () awful.util.spawn("amixer -q set Master toggle", false) end),
  awful.key({                   }, "XF86AudioRaiseVolume", function () awful.util.spawn("aumix -v+6", false) end),
  awful.key({                   }, "XF86AudioLowerVolume", function () awful.util.spawn("aumix -v-6", false) end),
  awful.key({                   }, "XF86Mail",             function () awful.util.spawn("mpc --no-status toggle", false) end),
  awful.key({                   }, "Print",                function () awful.util.spawn("scrot screenie-%H-%M-%d-%b.png -q 100", false) end),
  awful.key({ modkey,           }, "XF86Mail",             function () awful.util.spawn_with_shell("echo pause > ~/.mplayer/mplayer_fifo", false) end),
  awful.key({ modkey,           }, "End",                  function () awful.util.spawn("sudo shutdown -h now", false) end),
  awful.key({ modkey,           }, "Home",                 function () awful.util.spawn("sudo shutdown -r now", false) end),
  awful.key({ modkey,           }, "a",                    function () awful.util.spawn("apvlv", false) end),
  awful.key({ modkey,           }, "c",                    function () awful.util.spawn("chromium", false) end),
  awful.key({ modkey,           }, "d",                    function () awful.util.spawn("eject -T", false) end),
  awful.key({ modkey,           }, "f",                    function () awful.util.spawn("favsong", false) end),
  awful.key({ modkey,           }, "o",                    function () awful.util.spawn("oofice", false) end),
  awful.key({ modkey,           }, "r",                    function () awful.util.spawn("ranwall", false) end),
  awful.key({ modkey, "Control" }, "b",                    function () awful.util.spawn("favsong -b", false) end),
  awful.key({ modkey, "Control" }, "s",                    function () awful.util.spawn("mpc --no-status stop", false) end),
  awful.key({ modkey, "Control" }, "Left",                 function () awful.util.spawn("mpc --no-status prev", false) end),
  awful.key({ modkey, "Control" }, "Right",                function () awful.util.spawn("mpc --no-status next", false) end)
)

clientkeys = awful.util.table.join(
  awful.key({ modkey,           }, "b",      function (c) c.fullscreen = not c.fullscreen  end),
  awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
  awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
  awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
  keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
  globalkeys = awful.util.table.join(globalkeys,
    awful.key({ modkey }, "#" .. i + 9,
              function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewonly(tags[screen][i])
                end
              end),
    awful.key({ modkey, "Control" }, "#" .. i + 9,
              function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewtoggle(tags[screen][i])
                end
              end),
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
              function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.movetotag(tags[client.focus.screen][i])
                end
              end),
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
              function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.toggletag(tags[client.focus.screen][i])
                end
              end))
end

clientbuttons = awful.util.table.join(
  awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ modkey }, 1, awful.mouse.client.move),
  awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
  { rule = { },
    properties = { border_width = beautiful.border_width,
                   border_color = beautiful.border_normal,
                   focus = true,
                   size_hints_honor = false,
                   keys = clientkeys,
                   buttons = clientbuttons } },
  { rule = { class = "MPlayer" },
    properties = { floating = true, tag = tags[1][4], switchtotag = true } },
  { rule = { class = "XFontSel" },
    properties = { floating = true } },
  { rule = { class = "feh" },
    properties = { floating = true } },
  { rule = { class = "aumix" },
    properties = { floating = true } },
  { rule = { class = "Vlc" },
    properties = { floating = true, tag = tags[1][4], switchtotag = true } },
  { rule = { name = "Downloads" },
    properties = { floating = true } },
  { rule = { class = "Namoroka" },
    properties = { tag = tags[1][2], switchtotag = true } }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
  -- Add titlebar to floaters, but remove those from rule callback
  --if awful.client.floating.get(c)
  --or awful.layout.get(c.screen) == awful.layout.suit.floating then
  --  if   c.titlebar then awful.titlebar.remove(c)
  --  else awful.titlebar.add(c, {modkey = modkey}) end
  --end

  if not startup then
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Put windows in a smart way, only if they does not set an initial position.
    if not c.size_hints.user_position and not c.size_hints.program_position then
        awful.placement.no_overlap(c)
        awful.placement.no_offscreen(c)
    end
  end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- {{{ Arrange signal handler
for s = 1, screen.count() do screen[s]:add_signal("arrange", function ()
  local clients = awful.client.visible(s)
  local layout = awful.layout.getname(awful.layout.get(s))

  for _, c in pairs(clients) do -- Floaters are always on top
    if   awful.client.floating.get(c) or layout == "floating"
    then if not c.fullscreen then c.above       =  true  end
    else                          c.above       =  false end
  end
end)
end
-- }}}
-- }}}
