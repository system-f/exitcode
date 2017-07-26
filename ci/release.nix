let
  pkgs = import <nixpkgs> {};

  exitcode = import ../default.nix;

  jobs = rec {
    inherit exitcode;
  };
in
  jobs
