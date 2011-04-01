--[[    Background by londonali1010 (2009)
  VinDSL Background Hack (2010-2011)

This script draws a background to the Conky window. It covers the whole of the Conky window, but you can specify rounded corners, if you wish.

To call this script in Conky, use (assuming you have saved this script to ~/scripts/):
  lua_load ~/scripts/draw_bg.lua
  lua_draw_hook_pre draw_bg

Changelog:
  + v3.0        VinDSL Hack (01.28.2011) Killed memory leak.
  + v2.4        VinDSL Hack (01.25.2011) Declared all variables in local.
  + v2.3        VinDSL Hack (12.31.2010) Added shading example(s).
  + v2.2        VinDSL Hack (12.30.2010) Cleaned up the code a bit.
  + v2.1        VinDSL Hack (12.24.2010) Added cairo destroy function(s).
  + v2.0        VinDSL Hack (12.21.2010) Added height adjustment variable.
  + v1.0        Original release (07.10.2009)
]]

conky_background_color = 0x151515
conky_background_alpha = 0

ring_background_color  = 0x424242
ring_background_alpha  = 1
ring_foreground_color  = 0x86c113
ring_foreground_color2 = 0x1994d1
ring_foreground_color3 = 0xaa0000
ring_foreground_alpha  = 1

settings_table = {
    {
        name='time',
        arg='%S',
        max=60,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=300, y=1025,
        radius=50,
        thickness=5,
        start_angle=0,
        end_angle=360
    },
    {
        name='cpu',
        arg='cpu1',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=430, y=1025,
        radius=50,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='cpu',
        arg='cpu2',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color2,
        fg_alpha=ring_foreground_alpha,
        x=430, y=1025,
        radius=44,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='memperc',
        arg='',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=560, y=1025,
        radius=50,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='fs_used_perc',
        arg='/mnt/tv',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=680, y=1025,
        radius=50,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='fs_used_perc',
        arg='/stuff',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color2,
        fg_alpha=ring_foreground_alpha,
        x=680, y=1025,
        radius=44,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='fs_used_perc',
        arg='/home',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=680, y=1025,
        radius=38,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='fs_used_perc',
        arg='/',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color2,
        fg_alpha=ring_foreground_alpha,
        x=680, y=1025,
        radius=32,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='execi 3',
        arg='amixer get "Master Front" | egrep -o "[0-9]+%" | line | tr -d "%"',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=800, y=1025,
        radius=50,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='mpd_percent',
        arg='',
        max=100,
        bg_colour=ring_background_color,
        bg_alpha=ring_background_alpha,
        fg_colour=ring_foreground_color,
        fg_alpha=ring_foreground_alpha,
        x=1170, y=1025,
        radius=50,
        thickness=5,
        start_angle=-180,
        end_angle=90
    },
}

require 'cairo'

local cr = nil

local function rgb_to_r_g_b(colour,alpha)
    return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end

function draw_ring(cr,t,pt)
    local w,h=conky_window.width,conky_window.height

    local xc,yc,ring_r,ring_w,sa,ea=pt['x'],pt['y'],pt['radius'],pt['thickness'],pt['start_angle'],pt['end_angle']
    local bgc, bga, fgc, fga=pt['bg_colour'], pt['bg_alpha'], pt['fg_colour'], pt['fg_alpha']

    local angle_0=sa*(2*math.pi/360)-math.pi/2
    local angle_f=ea*(2*math.pi/360)-math.pi/2
    local t_arc=t*(angle_f-angle_0)

    -- Draw background ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_f)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(bgc,bga))
    cairo_set_line_width(cr,ring_w)
    cairo_stroke(cr)

    -- Draw indicator ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_0+t_arc)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(fgc,fga))
    cairo_stroke(cr)
end

function conky_ring_stats()
    local function setup_rings(cr,pt)
        local str=''
        local value=0

        str=string.format('${%s %s}',pt['name'],pt['arg'])
        str=conky_parse(str)

        value=tonumber(str)
        if value == nil then value = 0 end
        pct=value/pt['max']

        draw_ring(cr,pct,pt)
    end

    if conky_window==nil then return end
    local cs=cairo_xlib_surface_create(conky_window.display,conky_window.drawable,conky_window.visual, conky_window.width,conky_window.height)

    local cr=cairo_create(cs)

    local updates=conky_parse('${updates}')
    update_num=tonumber(updates)

    if update_num>5 then
        for i in pairs(settings_table) do
            setup_rings(cr,settings_table[i])
        end
    end
    cairo_destroy(cr)
end
