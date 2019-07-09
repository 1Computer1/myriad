# Myriad

Arbitrary code execution server using Docker.  

## Setup

- Fill out `config.dhall`, read it for documentation and an example
- Run `stack run`

## Endpoints

`GET /languages`  
List of enabled languages.  
Example response:  

```json
["haskell", "javascript"]
```

---

`POST /eval`  
Evaluate code.  
JSON payload with `language` and `code` keys.  
The `language` is as in the name of a subfolder in the `language` directory.  
Example payload:  

```json
{ "language": "haskell", "code": "main = print (1 + 1)" }
```

Example response:  

```json
{ "result": "2\n" }
```

Errors with 404 if `language` is not found, `504` if evaluation timed out, or `500` if evaluation failed for other reasons.  
