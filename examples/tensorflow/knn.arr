#######################
## Import Statements ##
#######################

import tensorflow as TF
import lists as L

######################
## Data Definitions ##
######################

type Tensor = TF.Tensor
type UserID = Number
type BookID = Number

data Book:
  | book(id :: BookID, title :: String, author :: String) with:
    method times-bought(self, users :: List<User>) -> Number:
      users.filter(lam(u): u.books-bought.member(self) end).length()
    end
end

data User:
  | user(id :: UserID, username :: String, books-bought :: List<Book>)
end

data Recommendation:
  | recommendation(book-data :: Book, dist :: Number)
end

data Distance:
  | distance(book-id :: Number, dist :: Number)
end

###################
## Main Function ##
###################

fun recommend(query-book :: Book, users :: List<User>, books :: List<Book>) -> List<Recommendation>:
  doc: ```
       Given a List<User> users and List<Book> books, returns the book that is
       most similar to query-book (this book must also be in the List of books)
       in terms of the buying patterns of Users who have bought the same Book
       ```
  matrix-buffer = build-matrix-buffer(users, books)
  query-tensor  = get-query-tensor(matrix-buffer, query-book, users.length())
  matrix        = matrix-buffer.to-tensor()

  dot-distances = TF.reduce-sum(matrix.multiply(query-tensor), some(1)).data-sync()
  get-recommendations(books, dot-distances)
end

fun get-recommendations(
    books :: List<Book>,
    num-distances :: List<Number>)
  -> List<Recommendation>:
  doc: ```
       Consumes a List<Book> books and a List<Number> num-distances where each
       element in num-distances represents the distance of the book at the
       corresponding index in books from the input query-book (from `recommend`)
       ```
  distances =
    for map_n(index from 0, dist from num-distances):
      distance(index, dist)
    end

  sorted-distances =
    distances.sort-by(
      lam(a, b): a.dist > b.dist end,
      lam(a, b): within(0.0000000001)(a.dist, b.dist) end)

  sorted-distances.map(
    lam(dist-object):
      found-book = find-book-with-id(books, dist-object.book-id)
      recommendation(found-book, dist-object.dist)
    end)
end

fun find-book-with-id(books :: List<Book>, query-id :: Number) -> Book:
  doc: "Returns the Book with the given query-id in the input List<Book> books"

  cases (List) books:
    | empty => raise("not found")
    | link(f, r) =>
      if f.id == query-id:
        f
      else:
        find-book-with-id(r, query-id)
      end
  end
end

fun get-query-tensor(
    matrix-buffer, # :: TensorBuffer
    query-book :: Book,
    num-users :: Number)
  -> Tensor:
  doc: ```
       Returns a unit vector Tensor that represents a Book, where all of the
       entries in the Tensor represent whether or not a User has bought that Book
       ```
  book-id = query-book.id

  data-list = for fold(base from [list:], user-id from range(0, num-users)):
    point = matrix-buffer.get([list: book-id, user-id])
    base.append([list: point])
  end

  TF.list-to-tensor(data-list)
end

fun build-matrix-buffer(users :: List<User>, books :: List<Book>): # -> TensorBuffer
  num-books = books.length()
  num-users = users.length()

  # Create a buffer with num-books rows and num-users columns:
  buffer = TF.make-buffer([list: num-books, num-users])

  _ = for each(usr from users):
    user-id      = usr.id
    books-bought = usr.books-bought
    for each(book-data from books-bought) block:
      book-id           = book-data.id
      times-bought      = book-data.times-bought(users)
      normalized-weight = 1 / times-bought
      buffer.set(normalized-weight, [list: book-id, user-id])
    end
  end

  buffer
end

fun generate-name(name-length :: NumNonNegative) -> String:
  doc: ```returns a random String name of length name-length, where
       the first letter of the name is uppercase and all subsequent
       letters are lowercase```

  fun generate-uppercase-letter() -> String:
    doc: "returns a random uppercase letter"

    # Code points for letters A - Z range from 65 - 90:
    code-point = num-random(26) + 65
    string-from-code-point(code-point)
  end

  fun generate-lowercase-letter() -> String:
    doc: "returns a random lowercase letter"

    # Code points for letters a - Z range from 97 - 122:
    code-point = num-random(26) + 97
    string-from-code-point(code-point)
  end

  if name-length <= 0:
    ""
  else if name-length == 1:
    generate-uppercase-letter()
  else:
    next-char = generate-name(name-length - 1)
    this-char = generate-lowercase-letter()
    next-char + this-char
  end
where:
  L.length(string-to-code-points(generate-name(0))) is 0
  L.length(string-to-code-points(generate-name(1))) is 1
  L.length(string-to-code-points(generate-name(7))) is 7
  L.length(string-to-code-points(generate-name(10))) is 10
end

############################
## Random Data Generators ##
############################

fun generate-random-books(num-books :: NumInteger) -> List<Book>:
  doc: "Generates a random list of Books"

  for fold(base from [list:], book-id from range(0, num-books)):
    title  = generate-name(num-random(15))
    author = generate-name(num-random(10))
    link(book(book-id, title, author), base)
  end
where:
  generate-random-books(0).length() is 0
  generate-random-books(1).length() is 1
  generate-random-books(2).length() is 2
  generate-random-books(5).length() is 5
  generate-random-books(10).length() is 10
end

fun generate-random-users(num-users :: NumInteger, books :: List<Book>) -> List<User>:
  doc: "Generates a random list of Users"

  num-books = books.length()
  for fold(base from [list:], user-id from range(0, num-users)):
    username     = generate-name(num-random(10))
    books-bought = L.shuffle(books).take(num-random(num-books))
    link(user(user-id, username, books-bought), base)
  end
where:
  books = generate-random-books(5)

  # Check for expected length:
  generate-random-users(0, books).length() is 0
  generate-random-users(1, books).length() is 1
  generate-random-users(2, books).length() is 2
  generate-random-users(5, books).length() is 5

  # Check that all books in each User came from the input list of books:
  ten-users = generate-random-users(10, books)
  ten-users.length() is 10
  ten-users.each(
    lam(usr :: User):
      usr.books-bought.each(
        lam(usr-book :: Book):
          usr-book satisfies books.member
        end)
    end)
end
