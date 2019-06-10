# logger_journald_backend

This library is a backend for Elixir's `Logger` which connects to Journald using its binary protocol and written in pure Erlang, without NIFs.
Journald is a part of systemd, an initialization and service supervision service for GNU/Linux, and is the simplest way to handle logging on modern Linux distributions, such as Ubuntu Server, CentOS 7, Arch Linux, or Debian.

To use it in a Phoenix application, add it as a backend in your `config/prod.exs` file:

```elixir
config :logger, level: :info,
  backends: [{:logger_journald_backend, :prod_log}]

# You can also use a custom formatter module, with the same API as Elixir Logger's
# formatter modules
config :logger, :prod_log, formatter: MyFormatterModule
```

You can then check your logs using the `journalctl` tool, which comes with systemd.
