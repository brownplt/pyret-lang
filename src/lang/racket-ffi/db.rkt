#lang pyret

import dblib as dblib

provide {
  DB: DB,
  sqlite: sqlite,
  Connection: Connection,
  connection: connection,
  Result: Result,
  simple_result: simple_result,
  rows_result: rows_result
}
end

List = list.List

data DB:
  # | postgres with:
  #     connect(user :: String, database :: String, password :: String):
  #       dblib.postgresql-connect(user, )
  #     end
  | sqlite with:
      connect(_, database :: String):
        connection(dblib.sqlite3-connect(database))
      end
end

data Connection:
  | connection(con) with:
      query(self, sql :: String, args :: List) -> Result:
        res = dblib.query(self.con, sql, args)
        if String(res): simple_result
        else if List(res): rows_result(res)
        end
      end,
      query_(self, sql :: String) -> Result:
        self.query(sql, [])
      end,
      connected(self):
        dblib.is-connected(self.con)
      end,
      disconnect(self):
        if dblib.is-connected(self.con):
          dblib.disconnect(self.con)
          nothing
        else:
          nothing
        end
      end
end

data Result:
  # racket makes it unclear what simple_results will return (and states it will
  # will change). So for now, just drop results.
  | simple_result
  | rows_result(rows :: List<List>)
end
