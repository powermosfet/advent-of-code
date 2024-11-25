{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/tarball/22.11") {}
}:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; with nodePackages; [
    purescript
    spago
    nodejs
    purescript-language-server
  ];

  shellHook = ''
    LOCALE_ARCHIVE="$(nix-build --no-out-link '<nixpkgs>' -A glibcLocales)/lib/locale/locale-archive"

    export LANG=en_GB.UTF-8
  '';
}
