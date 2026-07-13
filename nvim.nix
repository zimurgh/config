{ config, pkgs, lib, ... }:

let
  javaDebugExt = pkgs.vscode-extensions.vscjava.vscode-java-debug;
  javaTestExt = pkgs.vscode-extensions.vscjava.vscode-java-test;

  listJars = dir:
    lib.map (name: "${dir}/${name}") (
      lib.filter (name: lib.hasSuffix ".jar" name) (lib.attrNames (builtins.readDir dir))
    );

  javaBundles =
    (listJars "${javaDebugExt}/share/vscode/extensions/vscjava.vscode-java-debug/server")
    ++ (listJars "${javaTestExt}/share/vscode/extensions/vscjava.vscode-java-test/server");
in

{
  programs.nixvim = {
    enable = true;
    defaultEditor = true;
    nixpkgs.source = pkgs.path;

    colorschemes.tokyonight.enable = true;

    opts = {
      number = true;
      mouse = "a";
      hlsearch = false;
      expandtab = true;
      tabstop = 2;
      softtabstop = 2;
      shiftwidth = 2;
      smarttab = true;
      shiftround = true;
      lbr = true;
      ai = true;
      si = true;
      wrap = true;
      termguicolors = true;
    };

    plugins = {
      lualine = {
        enable = true;
        settings.options.theme = "tokyonight";
      };

      treesitter = {
        enable = true;
        nixGrammars = true;
        highlight.enable = true;
        indent.enable = true;
        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          rust
          java
          sql
          toml
          markdown
          lua
          bash
        ];
      };

      conform-nvim = {
        enable = true;
        autoInstall.enable = false;
        settings = {
          formatters_by_ft = {
            rust = [ "rustfmt" ];
            java = [ "google_java_format" ];
          };
          format_on_save = {
            timeout_ms = 500;
            lsp_format = "fallback";
          };
        };
      };

      jdtls = {
        enable = true;
        jdtLanguageServerPackage = pkgs.jdt-language-server;
        settings.init_options = {
          bundles = javaBundles;
          extendedClientCapabilities.__raw = "require('jdtls').extendedClientCapabilities";
        };
      };

      rustaceanvim = {
        enable = true;
        settings = {
          tools.reload_workspace_from_cargo_toml = true;
          server = {
            cmd = [ "rust-analyzer" ];
            default_settings = {
              "rust-analyzer" = {
                cargo = {
                  allFeatures = true;
                  loadOutDirsFromCheck = true;
                };
                checkOnSave = true;
                procMacro.enable = true;
              };
            };
            on_attach.__raw = ''
              function(_, bufnr)
                local opts = { buffer = bufnr, silent = true }

                vim.keymap.set("n", "<leader>ca", function()
                  vim.cmd.RustLsp("codeAction")
                end, vim.tbl_extend("force", opts, { desc = "Rust code action" }))

                vim.keymap.set("n", "<leader>rr", function()
                  vim.cmd.RustLsp("runnables")
                end, vim.tbl_extend("force", opts, { desc = "Rust runnables" }))

                vim.keymap.set("n", "<leader>rd", function()
                  vim.cmd.RustLsp("debuggables")
                end, vim.tbl_extend("force", opts, { desc = "Rust debuggables" }))

                vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", opts, { desc = "Hover" }))
              end
            '';
          };
          dap.autoload_configurations = true;
        };
      };

      dap = {
        enable = true;
      };

      dap-ui.enable = true;
      dap-virtual-text.enable = true;

      blink-cmp = {
        enable = true;
        settings = {
          keymap.preset = "enter";
          sources.default = [ "lsp" "path" "snippets" "buffer" ];
          completion.documentation.auto_show = true;
        };
      };

      web-devicons.enable = true;

      telescope = {
        enable = true;
        extensions.fzf-native.enable = true;
        keymaps = {
          "<leader>ff" = {
            action = "find_files";
            options.desc = "Find files";
          };
          "<leader>fg" = {
            action = "live_grep";
            options.desc = "Live grep";
          };
          "<leader>fb" = {
            action = "buffers";
            options.desc = "Buffers";
          };
          "<leader>fh" = {
            action = "help_tags";
            options.desc = "Help tags";
          };
          "<leader>fr" = {
            action = "oldfiles";
            options.desc = "Recent files";
          };
          "<leader>gf" = {
            action = "git_files";
            options.desc = "Git files";
          };
        };
      };

      gitsigns = {
        enable = true;
        settings.on_attach.__raw = ''
          function(bufnr)
            local gs = package.loaded.gitsigns

            local function map(mode, l, r, desc, extra)
              vim.keymap.set(mode, l, r, vim.tbl_extend("force", {
                buffer = bufnr,
                desc = desc,
                silent = true,
              }, extra or {}))
            end

            map("n", "]c", function()
              if vim.wo.diff then
                return "]c"
              end
              vim.schedule(function()
                gs.next_hunk()
              end)
              return "<Ignore>"
            end, "Git next hunk", { expr = true })

            map("n", "[c", function()
              if vim.wo.diff then
                return "[c"
              end
              vim.schedule(function()
                gs.prev_hunk()
              end)
              return "<Ignore>"
            end, "Git previous hunk", { expr = true })

            map({ "n", "v" }, "<leader>gs", function()
              gs.stage_hunk()
            end, "Git stage hunk")
            map({ "n", "v" }, "<leader>gr", function()
              gs.reset_hunk()
            end, "Git reset hunk")
            map("n", "<leader>gp", gs.preview_hunk, "Git preview hunk")
            map("n", "<leader>gb", function()
              gs.blame_line({ full = false })
            end, "Git blame line")
            map("n", "<leader>gB", function()
              gs.blame_line({ full = true })
            end, "Git blame buffer")
            map("n", "<leader>gd", gs.diffthis, "Git diff hunk")
            map("n", "<leader>gD", function()
              gs.diffthis("~")
            end, "Git diff file")
          end
        '';
      };

      lazygit.enable = true;

      dbee = {
        enable = true;
        settings = {
          default_connection = "dcim-postgres";
          sources = [
            {
              __raw = ''
                require("dbee.sources").MemorySource:new({
                  {
                    name = "dcim-postgres",
                    type = "postgres",
                    url = "postgres://dcim_user:abc.123@127.0.0.1:5432/dcim?sslmode=disable",
                  },
                  {
                    name = "dcim-mariadb",
                    type = "mysql",
                    url = "dcim_user:abc.123@tcp(127.0.0.1:3306)/dcim",
                  },
                })
              '';
            }
          ];
        };
      };
    };

    keymaps = [
      {
        key = "<leader>gg";
        action = "<cmd>LazyGit<cr>";
        options.desc = "LazyGit";
      }
      {
        key = "<leader>od";
        action = "<cmd>Dbee toggle<cr>";
        options.desc = "Database UI";
      }
    ];

    extraPlugins = with pkgs.vimPlugins; [
      crates-nvim
      vim-floaterm
      luasnip
    ];

    extraConfigLua = ''
      require("crates").setup({})

      local dap = require("dap")
      local dapui = require("dapui")

      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open({})
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close({})
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close({})
      end

      vim.keymap.set("n", "<F5>", dap.continue, { desc = "Debug: continue" })
      vim.keymap.set("n", "<F10>", dap.step_over, { desc = "Debug: step over" })
      vim.keymap.set("n", "<F11>", dap.step_into, { desc = "Debug: step into" })
      vim.keymap.set("n", "<F12>", dap.step_out, { desc = "Debug: step out" })
      vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Debug: toggle breakpoint" })
      vim.keymap.set("n", "<leader>dB", function()
        dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
      end, { desc = "Debug: conditional breakpoint" })
      vim.keymap.set("n", "<leader>du", dapui.toggle, { desc = "Debug: toggle UI" })
      vim.keymap.set("n", "<leader>dr", dap.repl.open, { desc = "Debug: open REPL" })

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("jdtls-dap", { clear = true }),
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if not client or client.name ~= "jdtls" then
            return
          end

          local jdtls = require("jdtls")
          jdtls.setup_dap({ hotcodereplace = "auto" })
          require("jdtls.dap").setup_dap_main_class_configs()

          local opts = { buffer = args.buf, silent = true }
          vim.keymap.set("n", "<leader>jc", jdtls.compile, vim.tbl_extend("force", opts, { desc = "Java compile" }))
          vim.keymap.set("n", "<leader>jo", function()
            vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } } })
          end, vim.tbl_extend("force", opts, { desc = "Java organize imports" }))
          vim.keymap.set("n", "<leader>jt", jdtls.test_class, vim.tbl_extend("force", opts, { desc = "Java debug test class" }))
          vim.keymap.set("n", "<leader>jT", jdtls.test_nearest_method, vim.tbl_extend("force", opts, { desc = "Java debug nearest test" }))
        end,
      })
    '';
  };
}
