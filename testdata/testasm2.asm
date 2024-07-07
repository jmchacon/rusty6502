; Now do a set in reverse order to cause multiple resolution loops
DDD = CCC
CCC = BBB
BBB = AAA
AAA = MSGX
MSGX = 0x1234

BEQ ABC
ABC = BCD
BCD = 0x00

HERE = *
