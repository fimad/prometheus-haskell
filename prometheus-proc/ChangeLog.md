# Next

* Change all use of `Int` to `Int64`. `Int` is only guaranteed to be able to
  represent values up to 2^29-1, but entries in /proc/stat can often exceed that
  (for example processes with a high resident memory usage) on 32-bit machines.

* Export `process_open_fds`, a count of the number of entries in
  `/proc/$PID/fd`.

* Export `process_max_fds`, which exports the soft ulimit of the maximum number
  of file descriptors this process may open.

# 0.1.0.0 -- 2019-01-15

* Initial release.
