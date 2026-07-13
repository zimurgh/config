{ config, pkgs, lib, ... }:

let
  dcimDb = "dcim";
  dcimUser = "dcim_user";
  dcimPassword = "abc.123";
in
{
  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    ensureDatabases = [ dcimDb ];
    settings.listen_addresses = lib.mkForce "127.0.0.1";
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 scram-sha-256
      host all all ::1/128 scram-sha-256
    '';
    ensureUsers = [
      {
        name = dcimUser;
        ensureClauses = {
          login = true;
          password = dcimPassword;
        };
      }
    ];
  };

  systemd.services.postgresql-dcim-grants = {
    description = "Grant dcim_user full access to the dcim PostgreSQL database";
    after = [ "postgresql-setup.service" ];
    requires = [ "postgresql-setup.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      RemainAfterExit = true;
    };
    path = [ config.services.postgresql.package ];
    script = ''
      psql -v ON_ERROR_STOP=1 <<'EOF'
      ALTER DATABASE ${dcimDb} OWNER TO ${dcimUser};
      GRANT ALL PRIVILEGES ON DATABASE ${dcimDb} TO ${dcimUser};
      EOF
      psql -d ${dcimDb} -v ON_ERROR_STOP=1 <<'EOF'
      GRANT ALL ON SCHEMA public TO ${dcimUser};
      ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO ${dcimUser};
      ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO ${dcimUser};
      EOF
    '';
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = [ dcimDb ];
    settings = {
      mysqld.bind-address = "127.0.0.1";
    };
  };

  systemd.services.mysql-dcim-user = {
    description = "Create dcim_user with password for MariaDB";
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
      FLUSH PRIVILEGES;
      EOF
    '';
  };
}
