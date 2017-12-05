type t = {
  mutable buff: bytes,
  mutable length: int
};

let make = () => {buff: Bytes.create(0), length: 0};

let with_capacity = (cap) => {buff: Bytes.create(cap), length: 0};

let clone = ({buff, length}) => {buff: Bytes.copy(buff), length};

let length = ({length}) => length;

let capacity = ({buff}) => Bytes.length(buff);

let resize = (self, new_size) => {
  assert (self.length <= new_size);
  let new_buff = Bytes.create(new_size);
  Bytes.blit(self.buff, 0, new_buff, 0, self.length);
  self.buff = new_buff;
};

let pop = (self) => {
  self.length = self.length - 1;
  Bytes.get(self.buff, self.length);
};

let push = (self, ch) => {
  if (self.length == capacity(self)) {
    resize(self, self.length * 2);
  };
  Bytes.set(self.buff, self.length, ch);
  self.length = self.length + 1;
};

let get = (self, idx) => {
  assert (idx < self.length);
  Bytes.get(self.buff, idx);
};

let set = (self, idx, ch) => {
  assert (idx < self.length);
  Bytes.set(self.buff, idx, ch);
};

let to_string = (self) => Bytes.sub_string(self.buff, 0, self.length);
