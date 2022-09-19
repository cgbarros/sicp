{ pkgs }: {
    deps = [
        pkgs.openssh_with_kerberos
        pkgs.bashInteractive
				pkgs.mitscheme
				pkgs.rlwrap
    ];
}