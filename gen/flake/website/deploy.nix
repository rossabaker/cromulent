{
  perSystem = { lib, pkgs, self', ... }:
    let
      deploy = pkgs.writeScript "deploy.sh" ''
        set -euo pipefail

        export PATH="${lib.makeBinPath [
          pkgs.coreutils
          pkgs.openssh
          pkgs.rsync
        ]}"

        echo "$SSH_PRIVATE_KEY" > ./ssh_key
        chmod 600 ./ssh_key

        rsync \
          -e "ssh -o StrictHostKeyChecking=no -i ./ssh_key" \
          -rvpc ${self'.packages.website}/ \
          --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
          --delete \
          www@abe.hetzner.rossabaker.com:/var/lib/www

        rm ./ssh_key
      '';
    in
    {
      apps.websiteDeploy.program = "${deploy}";
    };
}
