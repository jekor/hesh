# Hesh - The Haskell-Extensible Shell

Hesh makes writing scripts in Haskell easier. Here's an example:

```haskell
#!/usr/bin/env hesh
-- Backup a PostgreSQL database to an encrypted file.

main = do
  args <- System.Environment.getArgs
  case args of
    [database] -> do
      today <- $(date +%Y-%m-%d)
      let file = database ++ "-" ++ today ++ ".sql.gpg"
      $(pg_dump $database) |> $(gpg -e -r $EMAIL) /> file
    _ -> do
      progName <- System.Environment.getProgName
      $(echo "Usage: $progName <database>") /> "/dev/stderr"
```

For more details, see the [manual](manual/manual.pdf).
