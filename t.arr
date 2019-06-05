fun id(n :: {Number%<m>; Number%<_>}): n end
# TODO(benmusch): Why does this return such a weird contract error?
id({1%<m>; 2%<n>})
