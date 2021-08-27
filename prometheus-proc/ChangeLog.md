# 0.1.3.1 -- 2021-08-26

* Increase allowed prometheus-client version ranges.

# 0.1.3.0 -- 2020-06-17

* Support base-4.14 and GHC 8.10.

# 0.1.2.0 -- 2019-09-18

* Support base-4.13 and GHC 8.8.

# 0.1.1.0 -- 2019-05-23

* Change all use of `Int` to `Int64`. `Int` is only guaranteed to be able to
  represent values up to 2^29-1, but entries in /proc/stat can often exceed that
  (for example processes with a high resident memory usage) on 32-bit machines.

* Export `process_open_fds`, a count of the number of entries in
  `/proc/$PID/fd`.

* Export `process_max_fds`, which exports the soft ulimit of the maximum number
  of file descriptors this process may open.

# 0.1.0.0 -- 2019-01-15

* Initial release.
