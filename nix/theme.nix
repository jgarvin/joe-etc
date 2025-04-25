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
    kitty-themes
  ];

  programs.kitty = {
    themeFile = "ayu";
  };

  # Set up kvantum theme to match your GTK theme
  xdg.configFile = {
    "Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=Materia
      fixed=Iosevka,11,-1,5,50,0,0,0,0,0
      general=Iosevka,11,-1,5,50,0,0,0,0,0
    '';
    # Add the actual theme files
    "Kvantum/Materia".source = "${pkgs.materia-kde-theme}/share/Kvantum/Materia";

    # Qt5 configuration
    "qt5ct/qt5ct.conf".text = ''
    [Appearance]
    color_scheme_path=
    custom_palette=false
    icon_theme=Papirus-Dark
    standard_dialogs=default
    style=kvantum

    [Fonts]
    fixed="Iosevka,11,-1,5,50,0,0,0,0,0"
    general="Iosevka,11,-1,5,50,0,0,0,0,0"
  '';

    # Qt6 configuration
    "qt6ct/qt6ct.conf".text = ''
    [Appearance]
    color_scheme_path=
    custom_palette=false
    icon_theme=Papirus-Dark
    standard_dialogs=default
    style=kvantum

    [Fonts]
    fixed="Iosevka,11,-1,5,50,0,0,0,0,0"
    general="Iosevka,11,-1,5,50,0,0,0,0,0"
  '';

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