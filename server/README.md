prove-everywhere-server
=======================

This directory provides the server for ProveEverywhere.

You can also use this as a coqtop server.

Installation
------------

This is written in Haskell, so cabal is required in order to install.

```sh
$ cd /path/to/prove-everywhere/server
$ cabal install
```

Usage
-----

See: `prove-everywhere-server --help`

API
---

### PUT /start

This API starts new coqtop process and returns initial information.

#### example input

nothing to input

#### example output

```json
{
  "state": {
    "current_theorem": "Coq",
    "whole_state_number": 2,
    "theorem_stack": [],
    "theorem_state_number": 0
  },
  "output": "Welcome to Coq 8.4pl4 (June 2014)",
  "id": 2
}
```

### POST /command/:id

This API sends command to the coqtop and returns output from the coqtop.

#### example input

```json
{
  "command": "Require Import ssrbool."
}
```

#### example output

```json
{
  "remaining": 0,
  "state": {
    "current_theorem": "Coq",
    "whole_state_number": 3,
    "theorem_stack": [0],
    "theorem_state_number": 0
  },
  "error_output": null,
  "last_output": {
    "output": "Small Scale Reflection version 1.5 loaded. Copyright 2005-2012 Microsoft Corporation and INRIA. Distributed under the terms of the CeCILL-B license. [Loading ML file ssreflect.cmxs ... done] Ambiguous paths: [pred_of_mem_pred; sort_of_simpl_pred] : mem_pred >-> pred_sort",
    "type": "info"
  },
  "id": 0,
  "succeeded": 1
}
```

### DELETE /terminate/:id

This API kills the coqtop process.

#### example input

nothing to input

#### example output

```json
{}
```
