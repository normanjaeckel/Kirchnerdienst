{ pkgs, ... }:

{
  # See full reference at https://devenv.sh/reference/options/

  packages = [
      pkgs.git
      pkgs.go-task
      pkgs.go
      pkgs.zig_0_11
    ];

  enterShell = ''
    ROCDIR=$(pwd)/roc-dir
    PATH=$ROCDIR:$PATH
  '';
}
