{ config, pkgs, ... }:
{
  # Set up pointer cursor theme
  home.pointerCursor = {
    package = pkgs.capitaine-cursors;
    name = "capitaine-cursors";
    size = 24;
    # Enable for both X11 and GTK
    x11.enable = true;
    gtk.enable = true;
  };

  # Add session variables for Wayland
  home.sessionVariables = {
    XCURSOR_SIZE = "24";
    XCURSOR_THEME = "capitaine-cursors";
    # Force Qt to use Wayland
    QT_QPA_PLATFORM = "wayland;xcb";
    # Force GTK to use Wayland
    GDK_BACKEND = "wayland";
    # Set font rendering properties
    FREETYPE_PROPERTIES = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
  };

  # Configure GTK with Iosevka font
  gtk = {
    enable = true;
    theme = {
      name = "Materia";
      package = pkgs.materia-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    font = {
      name = "Iosevka";
      size = 11;
    };
  };

  # Qt configuration
  qt = {
    enable = true;
    platformTheme.name = "qtct";  # Using qt5ct/qt6ct for theme configuration
    style = {
      name = "kvantum";  # Better than "gtk2" for modern setups
    };
  };

  # Install necessary packages
  home.packages = with pkgs; [
    # Font
    iosevka
    # Qt theming
    libsForQt5.qt5ct
    qt6Packages.qt6ct
    libsForQt5.qtstyleplugin-kvantum
    qt6Packages.qtstyleplugin-kvantum
    # Theme packages
    materia-theme
    papirus-icon-theme
  ];

  # Set up kvantum theme to match your GTK theme
  xdg.configFile = {
    "Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=Materia
    '';
    # Add the actual theme files
    "Kvantum/Materia".source = "${pkgs.materia-kde-theme}/share/Kvantum/Materia";

    # Add fontconfig for more consistent font rendering
    "fontconfig/fonts.conf".text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
      <fontconfig>
        <match target="font">
          <edit name="antialias" mode="assign">
            <bool>true</bool>
          </edit>
          <edit name="hinting" mode="assign">
            <bool>true</bool>
          </edit>
          <edit name="hintstyle" mode="assign">
            <const>hintslight</const>
          </edit>
          <edit name="rgba" mode="assign">
            <const>rgb</const>
          </edit>
          <edit name="lcdfilter" mode="assign">
            <const>lcddefault</const>
          </edit>
        </match>
      </fontconfig>
    '';
  };
}