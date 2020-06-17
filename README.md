# Myriad

Arbitrary code execution server using Docker.  

## Requirements

You can use either `stack` or `cabal`.  
- `stack` should be >= 2.1.1, `cabal` should be >= 2.4.0.0.
- GHC 8.8.3 is required if not already installed by `stack` or if using `cabal`.

#### Install Binary

Make sure the place where `stack` or `cabal` places binaries is in your PATH.  
- For `stack`, you can get it with `stack path --local-bin`.
- For `cabal`, you should find it in `$HOME/.cabal/bin` (Linux) or `%APPDATA%\cabal\bin` (Windows).

Run `stack install` or `cabal new-install` inside the project folder.  
Make sure the configuration is filled out, see `config.example.yaml` for an example.  
Run `myriad --config /path/to/config.yaml --languages /path/to/languages/`.  

#### Install in Place

Make sure the configuration is filled out, see `config.example.yaml` for an example.  
Run `stack run` or `cabal v2-run` inside the project folder.  
The config and languages folder will default to `./config.yaml` and `./languages`.  
You can configure this with `--config` and `--languages`.  

## Endpoints

### **GET** `/languages`

List of enabled languages.  
Example response:  

```json
["haskell", "javascript"]
```

### **POST** `/eval`

Evaluate code.  
JSON payload with `language` and `code` keys.  
The `language` is as in the name of a subfolder in the `languages` directory.  
Example payload:  

```json
{ "language": "haskell", "code": "main = print (1 + 1)" }
```

Example response:  

```json
{ "result": "2\n" }
```

Errors with 404 if `language` is not found, `504` if evaluation timed out, or `500` if evaluation failed for other reasons.  

### **GET** `/containers`

List of containers being handled by Myriad.  

### **POST** `/cleanup`

Kill all containers, giving back the names of the containers killed.  
