{ config, pkgs, lib, ... }:

let
  dcimDb = "dcim";
  dcimUser = "dcim_user";
  dcimPassword = "abc.123";

  vcsDb = "vcs";
  vcsUser = "vcs_user";
  vcsPassword = "abc.123";
in
{

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = [ dcimDb vcsDb ];
    settings = {
      mysqld.bind-address = "127.0.0.1";
    };
  };

  systemd.services.mysql-app-users = {
    description = "Create MariaDB app users and grant database access";
    after = [ "mysql.service" ];
    requires = [ "mysql.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      ${pkgs.mariadb}/bin/mysql -u root --batch <<'EOF'
      CREATE USER IF NOT EXISTS '${dcimUser}'@'localhost' IDENTIFIED BY '${dcimPassword}';
      CREATE USER IF NOT EXISTS '${dcimUser}'@'127.0.0.1' IDENTIFIED BY '${dcimPassword}';
      GRANT ALL PRIVILEGES ON ${dcimDb}.* TO '${dcimUser}'@'localhost';
      GRANT ALL PRIVILEGES ON ${dcimDb}.* TO '${dcimUser}'@'127.0.0.1';

      CREATE USER IF NOT EXISTS '${vcsUser}'@'localhost' IDENTIFIED BY '${vcsPassword}';
      CREATE USER IF NOT EXISTS '${vcsUser}'@'127.0.0.1' IDENTIFIED BY '${vcsPassword}';
      GRANT ALL PRIVILEGES ON ${vcsDb}.* TO '${vcsUser}'@'localhost';
      GRANT ALL PRIVILEGES ON ${vcsDb}.* TO '${vcsUser}'@'127.0.0.1';

      FLUSH PRIVILEGES;
      EOF
    '';
  };
}
