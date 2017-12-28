
EMQ Retainer
============

The retainer plugin is responsible for storing retained MQTT messages.

Configuration
-------------

etc/emq_retainer.conf:

```
## Where to store the retained messages:
##  - ram: memory only
##  - disc: both memory and disc
##  - disc_only: disc only
##
## Enum values:
##  - ram, disc, disc_only
##
## Default: ram
##
## Notice: storage_type of each node in a cluster should be same.
retainer.storage_type = ram

## Maximum number of retained messages allowed.
retainer.max_message_num = 1000000

## Maximum payload size of a retained message.
retainer.max_payload_size = 64KB

## Expiration interval of the retained messages. Never expire if the value is 0.
##
## Duration values:
##  - h: hour
##  - m: minute
##  - s: second
##
## Examples:
##  - 2h:  2 hours
##  - 30m: 30 minutes
##  - 20s: 20 seconds
##
## Defaut: 0
retainer.expiry_interval = 0
```

License
-------

Apache License Version 2.0

Author
------

Feng at emqtt.io

