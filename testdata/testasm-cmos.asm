ORG $0F
LABEL EQU 0x40
BBR 0,$01,LABEL
TEST BBS 2,LABEL,$04
BBR 1,$FE,TEST