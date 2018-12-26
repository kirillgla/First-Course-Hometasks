Client/server-based image filtering.

Upon-connection protocol:
1. Server sends 1 byte: number of available filters
2. Server sends all filter names, 256 bytes each
3. Client sends filter name, 256 bytes
4. Client sends 4 bytes: image size in bytes
5. Client sends image data
6. Client may send 'stop' request as 1 byte of any value
7. Server sends:
   - 00 byte followed by another byte indicating progress
   - any other byte to indicate end of job, then goto 7
7. If client never sent stop signal, server sends resulting image
