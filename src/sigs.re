type io(+'a, 's);

type scheduler('s) = {
  bind: 'a 'b. (io('a, 's), 'a => io('b, 's)) => io('b, 's),
  return: 'a. 'a => io('a, 's),
};

type where =
  | SET
  | CUR
  | END;

type syscall('fd, 'error, 's) = {
  seek: ('fd, int64, where) => io(result(unit, 'error), 's),
  read: ('fd, int) => io(result(string, 'error), 's),
  line: 'fd => io(result((int, int, string), 'error), 's),
  read_int8: 'fd => io(result(int, 'error), 's),
  read_int16_ne: 'fd => io(result(int, 'error), 's),
  read_int32_ne: 'fd => io(result(int32, 'error), 's),
  read_int64_ne: 'fd => io(result(int64, 'error), 's),
};
