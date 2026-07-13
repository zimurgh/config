{ config, pkgs, lib, inputs, ... }:
{
  imports = [
    inputs.noctalia.homeModules.default
  ];

  programs.noctalia = {
    enable = true;
    systemd.enable = false;
    settings = {
      shell = {
        font_family = "FiraCode Nerd Font";
      };
      theme = {
        mode = "dark";
        source = "builtin";
        builtin = "Catppuccin";
      };
      wallpaper = {
        enabled = true;
        directory = "${config.home.homeDirectory}/Pictures/Wallpapers";
      };
    };
  };

  programs.niri = {
    enable = true;
    settings = {
      xwayland-satellite.path =
        lib.getExe inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.xwayland-satellite-unstable;

      input.keyboard.xkb = {
        layout = "us";
        variant = "dvp";
      };

      spawn-at-startup = [
        { argv = [ "noctalia" ]; }
      ];

      debug = {
        honor-xdg-activation-with-invalid-serial = [ ];
      };

      window-rules = [
        {
          geometry-corner-radius = {
            top-left = 20.0;
            top-right = 20.0;
            bottom-left = 20.0;
            bottom-right = 20.0;
          };
          clip-to-geometry = true;
        }
        {
          matches = [ { app-id = "dev.noctalia.Noctalia"; } ];
          open-floating = true;
          default-column-width.fixed = 1080;
          default-window-height.fixed = 920;
        }
        {
          matches = [ { app-id = "steam"; } ];
          clip-to-geometry = false;
        }
      ];

      layer-rules = [
        {
          matches = [ { namespace = "^noctalia-backdrop"; } ];
          place-within-backdrop = true;
        }
      ];

      binds = {
        "Mod+Space".action.spawn-sh = "noctalia msg panel-toggle launcher";
        "Mod+S".action.spawn-sh = "noctalia msg panel-toggle control-center";
        "Mod+Comma".action.spawn-sh = "noctalia msg settings-toggle";
        "XF86AudioRaiseVolume".action.spawn-sh = "noctalia msg volume-up";
        "XF86AudioLowerVolume".action.spawn-sh = "noctalia msg volume-down";
        "XF86AudioMute".action.spawn-sh = "noctalia msg volume-mute";
        "XF86MonBrightnessUp".action.spawn-sh = "noctalia msg brightness-up";
        "XF86MonBrightnessDown".action.spawn-sh = "noctalia msg brightness-down";
      };
    };
  };
}
