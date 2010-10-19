-- Awesome configuration, using awesome 3.4.8 on Arch GNU/Linux

-- {{{ libraries
require("awful")
require("awful.autofocus")
require("awful.rules")
require("beautiful")
require("naughty")
require("vicious")
require("scratch")
-- require("aware")
-- }}}

-- {{{ variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/vikas/theme.lua")

-- This is used later as the default terminal to run.
terminal = "urxvtc"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
  awful.layout.suit.floating,           --1
  awful.layout.suit.tile,               --2
  awful.layout.suit.tile.bottom,        --3
  awful.layout.suit.max,                --4
  awful.layout.suit.max.fullscreen,     --5
  awful.layout.suit.magnifier,          --6
  awful.layout.suit.fair.horizontal     --7
}
-- }}}

-- {{{ calendar function
local calendar = nil
local offset = 0

function remove_calendar()
  naughty.destroy(calendar)
  calendar = nil
  offset = 0
end

function add_calendar(inc_offset)
  local save_offset = offset
  if inc_offset == 0 and calendar ~= nil then
    remove_calendar()
    return
  end
  remove_calendar()
  offset = save_offset + inc_offset
  local datespec = os.date("*t")
  datespec = datespec.year * 12 + datespec.month - 1 + offset
  datespec = (datespec % 12 + 1) .. " " .. math.floor(datespec / 12)
  local cal = awful.util.pread("cal -s " .. datespec)
  cal = string.gsub(cal, "^%s*(.-)%s*$", "%1")
  calendar = naughty.notify({
    text = string.format('%s', "    "..cal),
    timeout = 0, hover_timeout = nil,
    width = 132,
  })
end
-- }}}

-- {{{ mpd cover art
local coverart_nf
function coverart()
  if coverart_nf ~= nil then
    naughty.destroy(coverart_nf)
    coverart_nf = nil
    return
  end
  local img = image(awful.util.pread("cover_art"))
  local txt = awful.util.pread("mpdinfo")
  coverart_nf = naughty.notify({icon = img, icon_size = 100, text = txt, timeout = 0})
end
-- }}}

-- {{{ naughty configuration
naughty.config.presets.normal.timeout          = 5
naughty.config.presets.normal.font             = beautiful.font
naughty.config.presets.normal.ontop            = true
naughty.config.presets.normal.fg               = '#f0dfaf'
naughty.config.presets.normal.bg               = '#1a1a1a'
naughty.config.presets.normal.border_color     = beautiful.border_focus
naughty.config.presets.normal.border_width     = 1
-- }}}

-- {{{ tags
-- Define a tag table which will hold all screen tags.
tags = {
  names  = { "sys", "web", "term", "media", "code" },
  layout = { layouts[3], layouts[6], layouts[4], layouts[1], layouts[2]
}}

for s = 1, screen.count() do
  -- Each screen has its own tag table.
  tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ wibox

-- {{{ defaults
spacer    = widget({ type = "textbox"  })
separator = widget({ type = "textbox" })
spacer.text     = " "
separator.text  = "|"
-- }}}

-- {{{ widgets

-- {{{ CPU usage
cpuicon       = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph  = awful.widget.graph()
-- Graph properties
cpugraph:set_width(50)
cpugraph:set_height(12)
cpugraph:set_background_color(beautiful.fg_off_widget)
cpugraph:set_color(beautiful.fg_widget)
cpugraph:set_gradient_angle(0)
cpugraph:set_gradient_colors({ beautiful.fg_end_widget,
   beautiful.fg_center_widget, beautiful.fg_widget
})
-- Register widgets
vicious.register(cpugraph, vicious.widgets.cpu, "$1")
-- }}}

-- {{{ Memory usage
memicon       = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)
-- Initialize widget
memwidget     = widget({ type = "textbox" })
vicious.cache(vicious.widgets.mem)
-- Register widget
vicious.register(memwidget, vicious.widgets.mem, '$2M', 11)
-- }}}

-- {{{ File system usage
fsicon       = widget({ type = "imagebox" })
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
  w:set_border_color(beautiful.bg_widget)
  w:set_color(beautiful.fg_widget)
  w:set_gradient_colors({ beautiful.fg_widget,
     beautiful.fg_center_widget, beautiful.fg_end_widget
  })
end
-- caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",       599)
vicious.register(fs.h, vicious.widgets.fs, "${/home used_p}",   599)
vicious.register(fs.t, vicious.widgets.fs, "${/mnt/tv used_p}", 599)
vicious.register(fs.s, vicious.widgets.fs, "${/stuff used_p}",  599)
-- }}}

-- {{{ Volume level
volicon       = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
-- Progressbar properties
volbar:set_width(50)
volbar:set_height(12)
volbar:set_vertical(false):set_ticks(true):set_ticks_size(2)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_color(beautiful.fg_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
})
-- Register widgets
vicious.register(volbar, vicious.widgets.volume,
  function (widgets, args)
    if args[1] == 0 then
      volicon.image = image(beautiful.widget_vol_mute)
      return 0
    else
      volicon.image = image(beautiful.widget_vol)
      return args[1]
    end
  end, 2, "Master")
-- }}}

-- {{{ Date and time
dateicon       = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
-- Initialize widget
datewidget = widget({ type = "textbox" })
datewidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () add_calendar(0) end),
  awful.button({ }, 4, function () add_calendar(-1) end),
  awful.button({ }, 5, function () add_calendar(1) end)
))
-- Register widget
vicious.register(datewidget, vicious.widgets.date, '<span color="khaki">%a %d %b, %H:%M</span>', 61)
-- }}}

--{{{ mail
mailicon       = widget({ type = "imagebox" })
mailicon.image = image(beautiful.widget_mail)
mailwidget = widget({ type = 'textbox' })
vicious.register(mailwidget, vicious.widgets.mdir,
  function (widget, args)
    if args[1] == 0 then
      mailicon.image = image(beautiful.widget_mail)
      return 0
    else
      mailicon.image = image(beautiful.widget_mail_new)
      return args[1]
    end
  end, 113, {"/home/vikki/mail/inbox"})
--}}}

-- hddtemp {{{
-- initialize
hdd = {
  sda = widget({ type = "textbox" }),
  sdb = widget({ type = "textbox" })
}
-- caching
vicious.cache(vicious.widgets.hddtemp)
-- register
vicious.register(hdd.sda, vicious.widgets.hddtemp, '<span color="#659fdb">sda</span> ${/dev/sda}C', 53)
vicious.register(hdd.sdb, vicious.widgets.hddtemp, '<span color="#659fdb">sdb</span> ${/dev/sdb}C', 59)
-- }}}

-- mpd {{{
mpdwidget = widget({ type = 'textbox' })
-- register & custom o/p fn
vicious.register(mpdwidget, vicious.widgets.mpd,
  function (widget, args)
    if   args["{state}"] == 'Stop' then
      return
    elseif args["{state}"] == 'Pause' then
      return '| <span color="#f8072e">' .. args["{Artist}"] .. '</span> - <span color="#f8072e">' .. args["{Title}"] .. '</span>'
    else
      return '| <span color="#5bde58">' .. args["{Artist}"] .. '</span> - <span color="#5bde58">' .. args["{Title}"] .. '</span>'
    end
  end)
-- }}}

-- {{{ uptime
uptimewidget = widget({ type = 'textbox' })
vicious.register(uptimewidget, vicious.widgets.uptime,
  function (widget, args)
    return string.format('<span color="#f8b009">%1dd %02dh %02dm</span>', args[1], args[2], args[3])
  end, 61)
-- }}}

-- {{{ pkg updates
updateicon    = widget({ type = "imagebox" })
updatewidget  = widget({ type = 'textbox' })
updateicon.image = image(beautiful.widget_pacman)
vicious.register(updatewidget, vicious.widgets.pkg,
  function (widget, args)
    if args[1] == 0 then
      updateicon.image = image(beautiful.widget_pacman)
      return 0
    elseif args[1] <= 30 then
      updateicon.image = image(beautiful.widget_pacman_new)
      return args[1]
    else
      updateicon.image = image(beautiful.widget_pacman_new_more)
      return args[1]
    end
  end, 3607, 'Arch')
-- }}}

-- {{{ rssbox
-- rssbox = widget({ type = "textbox" })
-- aware.register(rssbox, { "http://rss.feedsportal.com/c/32569/f/491734/index.rss" }) -- http://www.h-online.com/grand-atom.xml, http://rss.feedsportal.com/c/32569/f/491734/index.rss
-- }}}

-- {{{ ip box
-- ipbox = widget({ type = "textbox" })
-- }}}

-- {{{ systray
mysystray = widget({ type = "systray" })
-- }}}

-- }}}

-- {{{ lists and boxes
top_wibox = {}
bottom_wibox = {}
mytaglist = {}
mytasklist = {}
mylayoutbox = {}
mypromptbox = {}
-- }}}

-- {{{  add each widget
for s = 1, screen.count() do
  -- Create a promptbox for each screen
  mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })

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
    bg = beautiful.bg_normal, position = "top",
    border_color = beautiful.border_focus,
    border_width = beautiful.border_width
  })

--  bottom_wibox[s] = awful.wibox({
--    screen = s,
--    fg = beautiful.fg_normal, height = 12,
--    bg = beautiful.bg_normal,
--    position = "bottom"
--  })

  -- Add widgets to the wibox - order matters
  top_wibox[s].widgets = {
    {
      spacer, uptimewidget, spacer,
      separator, spacer, mytaglist[s], spacer,
      mylayoutbox[s], spacer,
      separator, spacer, mypromptbox[s],
      layout = awful.widget.layout.horizontal.leftright
    },
    spacer,    datewidget, spacer, dateicon, spacer,
    separator, spacer, volbar.widget, spacer, volicon, spacer,
    separator, spacer, hdd.sdb, spacer, separator, spacer, hdd.sda, spacer,
    separator, spacer, fs.t.widget, fs.s.widget, fs.h.widget, fs.r.widget, spacer, fsicon, spacer,
    separator, spacer, mailwidget, spacer, mailicon, spacer,
    separator, spacer, memwidget, spacer, memicon, spacer,
    separator, spacer, cpugraph.widget, spacer, cpuicon, spacer,
    separator, spacer, updatewidget, spacer, updateicon, spacer,
--    separator, spacer, ipbox, spacer,
    separator, spacer, mpdwidget,
    s == 1 and mysystray or nil,
    mytasklist[s],
    layout = awful.widget.layout.horizontal.rightleft
  }

--  bottom_wibox[s].widgets = {
--    spacer, rssbox, spacer,
--    layout = awful.widget.layout.horizontal.leftright
--  }
end
-- }}}

-- }}}

-- {{{ key bindings
globalkeys = awful.util.table.join(
  awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

  -- scratchpad
  awful.key({                   }, "F12",    function () scratch.drop(terminal .. " -name scratch", "center", "center", 1200, .40) end),
  awful.key({ modkey            }, "s",      function () scratch.pad.toggle() end),

  awful.key({ modkey            }, "Tab",    function () awful.client.focus.byidx( 1) if client.focus then client.focus:raise() end end),
  awful.key({ modkey, "Shift"   }, "Tab",    function () awful.client.focus.byidx(-1) if client.focus then client.focus:raise() end end),
  awful.key({ modkey            }, "j",      function () awful.client.focus.history.previous() if client.focus then client.focus:raise() end end),

  -- Layout manipulation
  awful.key({ modkey, "Shift"   }, "j",      function () awful.client.swap.byidx(  1)    end),
  awful.key({ modkey, "Shift"   }, "k",      function () awful.client.swap.byidx( -1)    end),
  awful.key({ modkey, "Control" }, "j",      function () awful.screen.focus_relative( 1) end),
  awful.key({ modkey, "Control" }, "k",      function () awful.screen.focus_relative(-1) end),

  awful.key({ modkey            }, "u",      awful.client.urgent.jumpto),

  -- Standard program
  awful.key({ modkey, "Shift"   }, "Return", function () awful.util.spawn(terminal) end),
  awful.key({ modkey            }, "Return", function () awful.util.spawn(terminal .. " -name main") end),
  awful.key({ modkey, "Control" }, "r",      awesome.restart),
  awful.key({ modkey, "Shift"   }, "q",      awesome.quit),

  awful.key({ modkey            }, "l",      function () awful.tag.incmwfact( 0.05)    end),
  awful.key({ modkey,           }, "h",      function () awful.tag.incmwfact(-0.05)    end),
  awful.key({ modkey, "Shift"   }, "h",      function () awful.tag.incnmaster( 1)      end),
  awful.key({ modkey, "Shift"   }, "l",      function () awful.tag.incnmaster(-1)      end),
  awful.key({ modkey, "Control" }, "h",      function () awful.tag.incncol( 1)         end),
  awful.key({ modkey, "Control" }, "l",      function () awful.tag.incncol(-1)         end),
  awful.key({ modkey            }, "space",  function () awful.layout.inc(layouts,  1) end),
  awful.key({ modkey, "Shift"   }, "space",  function () awful.layout.inc(layouts, -1) end),

  -- Prompt
  awful.key({ modkey            }, "p",      function () mypromptbox[mouse.screen]:run() end),
  awful.key({ modkey            }, "x",      function ()
                                               awful.prompt.run({ prompt = "run lua code: " },
                                               mypromptbox[mouse.screen].widget,
                                               awful.util.eval, nil,
                                               awful.util.getdir("cache") .. "/history_eval")
                                             end),

  awful.key({ modkey            }, "F12",    function ()
                                               awful.prompt.run({ prompt = "web search: " },
                                               mypromptbox[mouse.screen].widget,
                                               function (command)
                                                 awful.util.spawn("firefox 'http://yubnub.org/parser/parse?command="..command.."'", false)
                                                 -- Switch to the web tag, where Firefox is, in this case tag 2
                                                 if tags[mouse.screen][2] then awful.tag.viewonly(tags[mouse.screen][2]) end
                                               end)
                                             end),

  -- custom apps keys
  awful.key({                   }, "XF86Search",           function () awful.util.spawn("firefox", false) end),
  awful.key({                   }, "XF86Sleep",            function () awful.util.spawn("alock -auth md5:file=/home/vikki/mydocs/passphrase", false) end),
  awful.key({                   }, "XF86AudioMute",        function () awful.util.spawn("mute", false) end),
  awful.key({                   }, "XF86AudioRaiseVolume", function () awful.util.spawn("aumix -v+6", false) end),
  awful.key({                   }, "XF86AudioLowerVolume", function () awful.util.spawn("aumix -v-6", false) end),
  awful.key({                   }, "XF86Mail",             function () awful.util.spawn("ncmpcpp toggle", false) end),
  awful.key({                   }, "Print",                function () awful.util.spawn("scrot screenie-%H-%M-%S-%d-%b.png -q 100", false) end),
  awful.key({ modkey            }, "XF86Mail",             function () awful.util.spawn_with_shell("echo pause > ~/.mplayer/mplayer_fifo", false) end),
  awful.key({ modkey            }, "End",                  function () awful.util.spawn_with_shell("sudo shutdown -h now", false) end),
  awful.key({ modkey            }, "Home",                 function () awful.util.spawn_with_shell("sudo shutdown -r now", false) end),
  awful.key({ modkey            }, "a",                    function () awful.util.spawn("evince", false) end),
  awful.key({ modkey            }, "c",                    function () awful.util.spawn("chromium", false) end),
  awful.key({ modkey            }, "e",                    function () awful.util.spawn("eject -T", false) end),
  awful.key({ modkey            }, "i",                    function () awful.util.spawn("inkscape", false) end),
  awful.key({ modkey            }, "f",                    function () awful.util.spawn("favsong", false) end),
  awful.key({ modkey            }, "z",                    function () coverart() end),
  awful.key({ modkey            }, "o",                    function () awful.util.spawn("ooffice", false) end),
  awful.key({ modkey            }, "r",                    function () awful.util.spawn("ranwall", false) end),
  awful.key({ modkey, "Control" }, "b",                    function () awful.util.spawn("favsong -b", false) end),
  awful.key({ modkey, "Control" }, "s",                    function () awful.util.spawn("ncmpcpp stop", false) end),
  awful.key({ modkey, "Control" }, "Left",                 function () awful.util.spawn("ncmpcpp prev", false) end),
  awful.key({ modkey, "Control" }, "Right",                function () awful.util.spawn("ncmpcpp next", false) end)
)

clientkeys = awful.util.table.join(
  awful.key({ modkey            }, "b",      function (c) c.fullscreen = not c.fullscreen  end),
  awful.key({ modkey            }, "d",      function (c) scratch.pad.set(c, 0.70, 0.70)   end),
  awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
  awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
  awful.key({ modkey, "Shift"   }, "s",      function (c) c.sticky = not c.sticky          end),
  awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
  awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     )
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

-- {{{ rules
awful.rules.rules = {
  { rule = { },
    properties = { focus = true,
                   size_hints_honor = false,
                   keys = clientkeys,
                   buttons = clientbuttons } },
  { rule = { instance = "main" },
    properties = { floating = true,
    border_color = beautiful.border_focus,
    border_width = beautiful.border_width },
    callback = awful.placement.centered },
  { rule = { instance = "scratch" },
    properties = { border_color = beautiful.border_focus,
    border_width = beautiful.border_width } },
  { rule = { instance = "Navigator" },
    properties = { tag = tags[1][2], switchtotag = true } },
  { rule_any = { class = { "XFontSel", "Gimp" }, name = { "Downloads" } },
    properties = { floating = true } },
  { rule_any = { class = { "MPlayer", "Vlc" } },
    properties = { floating = true, tag = tags[1][4], switchtotag = true } }
}
-- }}}

-- {{{ signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
  if not startup then
    -- Put windows in a smart way, only if they does not set an initial position.
    if not c.size_hints.user_position and not c.size_hints.program_position then
        awful.placement.no_offscreen(c)
    end
  end
end)

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
