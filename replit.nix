{ pkgs }: {
    deps = [
      pkgs.unzip
        pkgs.vim
        pkgs.openssh_with_kerberos
        pkgs.bashInteractive
		pkgs.mitscheme
		pkgs.rlwrap
    ];
}