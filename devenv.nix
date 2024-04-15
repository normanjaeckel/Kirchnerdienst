{ pkgs, lib, config, inputs, ... }:

{
  # See full reference at https://devenv.sh/reference/options/

  packages = [
      pkgs.git
      pkgs.go-task
    ];

  enterShell = ''
    ROCDIR=$(pwd)/roc-dir
    PATH=$ROCDIR:$PATH
  '';
}
