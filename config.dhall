let LanguageConfig : Type =
    { name       : Text    -- Name of language as in languages folder
    , memory     : Text    -- Maximum memory usage
    , cpus       : Text    -- Maximum CPU usage
    , timeout    : Natural -- Timeout for code evaluation in seconds
    , concurrent : Natural -- Maximum number of concurrent evaluations
    , retries    : Natural -- Maximum number of retries for unsure errors
    }

-- Creates the default configuration given a language name.
-- For more customization, use the (//) operator e.g. cfg "haskell" // { timeout = 20 } or write the full record out.
let cfg = \(name : Text) ->
    { name       = name
    , memory     = "256m"
    , cpus       = "0.25"
    , timeout    = 20
    , concurrent = 10
    , retries    = 2
    }

let Config : Type =
    { languages         : List LanguageConfig -- List of languages to enable
    , buildConcurrently : Bool                -- Whether to build images concurrently
    , prepareContainers : Bool                -- Whether to setup all containers on startup
    , cleanupInterval   : Natural             -- The interval in minutes to kill containers periodically
    , port              : Natural             -- Port to run on
    , languagesDir      : Text                -- Where the languages are stored
    }

-- Write your config here!
let config : Config =
    { languages =
        [ cfg "javascript"
        ]
    , buildConcurrently = True
    , prepareContainers = False
    , cleanupInterval   = 30
    , port              = 8081
    , languagesDir      = "./languages"
    }
in config
