{ config, lib, pkgs, inputs, unstablePkgs, ... }:

{
  # OpenCode config managed declaratively. Plugin packages listed here are
  # fetched by opencode itself from npm at startup (same mechanism as the
  # Pi agent's packages list), so no nix-side node_modules needed.
  #
  # @prevalentware/opencode-goal-plugin adds the /goal command: persistent
  # long-running goals with auto-continue, evidence-gated completion, and
  # a TUI sidebar indicator. It needs an entry in both V1 config targets.
  home.file.".config/opencode/opencode.jsonc" = {
    force = true;
    text = builtins.toJSON {
      "$schema" = "https://opencode.ai/config.json";
      plugin = [
        "@prevalentware/opencode-goal-plugin"
        "opencode-auto-permissions"
      ];
      permission = {
        bash = {
          "*" = "ask";
          "pwd" = "allow";
          "ls*" = "allow";
          "rg *" = "allow";
          "grep *" = "allow";
          "cat *" = "allow";
          "head *" = "allow";
          "tail *" = "allow";
          "wc *" = "allow";
          "stat *" = "allow";
          "file *" = "allow";
          "date" = "allow";
          "hostname" = "allow";
          "ps *" = "allow";
          "git status*" = "allow";
          "git diff*" = "allow";
          "git log*" = "allow";
          "git show*" = "allow";
          "git ls-files*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh pwd" = "allow";
          "/home/prophet/etc/shell/agent_run.sh ls*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh rg *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh grep *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh cat *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh head *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh tail *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh wc *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh stat *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh file *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh date" = "allow";
          "/home/prophet/etc/shell/agent_run.sh hostname" = "allow";
          "/home/prophet/etc/shell/agent_run.sh ps *" = "allow";
          "/home/prophet/etc/shell/agent_run.sh git status*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh git diff*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh git log*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh git show*" = "allow";
          "/home/prophet/etc/shell/agent_run.sh git ls-files*" = "allow";
        };
        external_directory = "ask";
        webfetch = "allow";
        websearch = "allow";
      };
    };
  };

  home.file.".config/opencode/tui.json" = {
    force = true;
    text = builtins.toJSON {
      "$schema" = "https://opencode.ai/tui.json";
      plugin = [
        "@prevalentware/opencode-goal-plugin"
        "opencode-auto-permissions"
      ];
      keybinds = {
        messages_first = "ctrl+a";
        messages_last = "ctrl+e";
        input_buffer_home = "home";
        input_buffer_end = "end";
      };
    };
  };
}
