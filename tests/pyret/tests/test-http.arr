import http as HTTP

hostname = 'code.pyret.org'
id = '0Bz8lcSxx1xz6TEl2RWtxOFBsaFU'
path = '/shared-program-contents?sharedProgramId=' + id

res = HTTP.get(hostname, path)
print('Contents of shared program:\n')
print(string-repeat('-', 80) + '\n')
print(res.rsp-body)
print('\n')
print(string-repeat('-', 80) + '\n\n')
