      * COPYMEMBER: LIBGB/HEX.CMD
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/HEX.CMD
      *
      * DESCRIPTION   : HEX CODES
      *
      *=================================================================
      * REV:
      * KEC 2002.1002 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      * JKC 2024-0919 ADDED HEX-TAB TO MAKE IT STANDARD.
      *               FIXED THE FOLLOWING SINCE ALL VALUED TO X"3A":
      *                 HEX-SEMI-COLON
      *                 HEX-LESS-THAN
      *                 HEX-EQUAL
      *                 HEX-GREATER-THAN
      *                 HEX-QUESTION
      *                 HEX-AT
      *
      *=================================================================
      *
      *   CHARACTER   HEX   OCT   FIELD-NAME
      *   ---------   ---   ---   ------------------------------
      *
      *   NUL         00    000   HEX-NUL
      *   SOH         01    001   HEX-SOH
      *   STX         02    002   HEX-STX
      *   ETX         03    003   HEX-ETX
      *   EOT         04    004   HEX-EOT
      *   ENQ         05    005   HEX-ENQ
      *   ACK         06    006   HEX-ACK
      *   BEL         07    007   HEX-BEL
      *   BS          08    010   HEX-BS
      *   HT          09    011   HEX-HT
      *   NL          0A    012   HEX-NL
      *   VT          0B    013   HEX-VT
      *   NP          0C    014   HEX-NP
      *   CR          0D    015   HEX-CR
      *   SO          0E    016   HEX-SO
      *   SI          0F    017   HEX-SI
      *   DLE         10    020   HEX-DLE
      *   DC1         11    021   HEX-DC1
      *   DC2         12    022   HEX-DC2
      *   DC3         13    023   HEX-DC3
      *   DC4         14    024   HEX-DC4
      *   NAK         15    025   HEX-NAK
      *   SYN         16    026   HEX-SYN
      *   ETB         17    027   HEX-ETB
      *   CAN         18    030   HEX-CAN
      *   EM          19    031   HEX-EM
      *   SUB         1A    032   HEX-SUB
      *   ESC         1B    033   HEX-ESC
      *                           HEX-EC
      *                           HEX-ESCAPE
      *   FS          1C    034   HEX-FS
      *   GS          1D    035   HEX-GS
      *   RS          1E    036   HEX-RS
      *   US          1F    037   HEX-US
      *   SP          20    040   HEX-SP
      *                           HEX-SPACE
      *   !           21    041   HEX-EXCLAMATION
      *                           HEX-EXCLAMATION-POINT
      *   "           22    042   HEX-DOUBLE-QUOTE
      *   #           23    043   HEX-NUMBER
      *                           HEX-NUMBER-SIGN
      *   $           24    044   HEX-DOLLAR
      *                           HEX-DOLLAR-SIGN
      *   %           25    045   HEX-PERCENT
      *                           HEX-PERCENT-SIGN
      *   &           26    046   HEX-AND
      *                           HEX-AND-SIGN   
      *                           HEX-AMPERSAND
      *   '           27    047   HEX-SINGLE-QUOTE
      *   (           28    050   HEX-LEFT-PARENTHESIS
      *   )           29    051   HEX-RIGHT-PARENTHESIS
      *   *           2A    052   HEX-ASTERISK
      *   +           2B    053   HEX-PLUS
      *   ,           2C    054   HEX-COMMA
      *   -           2D    055   HEX-MINUS
      *                           HEX-DASH     
      *                           HEX-HYPHEN   
      *   .           2E    056   HEX-PERIOD
      *   /           2F    057   HEX-SLASH
      *                           HEX-FORWARD-SLASH
      *   0           30    060   HEX-0
      *   1           31    061   HEX-1
      *   2           32    062   HEX-2
      *   3           33    063   HEX-3
      *   4           34    064   HEX-4
      *   5           35    065   HEX-5
      *   6           36    066   HEX-6
      *   7           37    067   HEX-7
      *   8           38    070   HEX-8
      *   9           39    071   HEX-9
      *   :           3A    072   HEX-COLON
      *   ;           3B    073   HEX-SEMI-COLON
      *   <           3C    074   HEX-LESS-THAN
      *   =           3D    075   HEX-EQUAL
      *   >           3E    076   HEX-GREATER-THAN
      *   ?           3F    077   HEX-QUESTION
      *                           HEX-QUESTION-MARK
      *   @           40    100   HEX-AT
      *                           HEX-AT-SIGN
      *   A           41    101   HEX-UPPER-A
      *   B           42    102   HEX-UPPER-B
      *   C           43    103   HEX-UPPER-C
      *   D           44    104   HEX-UPPER-D
      *   E           45    105   HEX-UPPER-E
      *   F           46    106   HEX-UPPER-F
      *   G           47    107   HEX-UPPER-G
      *   H           48    110   HEX-UPPER-H
      *   I           49    111   HEX-UPPER-I
      *   J           4A    112   HEX-UPPER-J
      *   K           4B    113   HEX-UPPER-K
      *   L           4C    114   HEX-UPPER-L
      *   M           4D    115   HEX-UPPER-M
      *   N           4E    116   HEX-UPPER-N
      *   O           4F    117   HEX-UPPER-O
      *   P           50    120   HEX-UPPER-P
      *   Q           51    121   HEX-UPPER-Q
      *   R           52    122   HEX-UPPER-R
      *   S           53    123   HEX-UPPER-S
      *   T           54    124   HEX-UPPER-T
      *   U           55    125   HEX-UPPER-U
      *   V           56    126   HEX-UPPER-V
      *   W           57    127   HEX-UPPER-W
      *   X           58    130   HEX-UPPER-X
      *   Y           59    131   HEX-UPPER-Y
      *   Z           5A    132   HEX-UPPER-Z
      *   [           5B    133   HEX-LEFT-BRACKET
      *   \           5C    134   HEX-BACK-SLASH
      *   ]           5D    135   HEX-RIGHT-BRACKET
      *   ^           5E    136   HEX-CARET
      *   _           5F    137   HEX-UNDER-SCORE
      *   *unknown*   60    140   HEX-
      *   a           61    141   HEX-LOWER-A
      *   b           62    142   HEX-LOWER-B
      *   c           63    143   HEX-LOWER-C
      *   d           64    144   HEX-LOWER-D
      *   e           65    145   HEX-LOWER-E
      *   f           66    146   HEX-LOWER-F
      *   g           67    147   HEX-LOWER-G
      *   h           68    150   HEX-LOWER-H
      *   i           69    151   HEX-LOWER-I
      *   j           6A    152   HEX-LOWER-J
      *   k           6B    153   HEX-LOWER-K
      *   l           6C    154   HEX-LOWER-L
      *   m           6D    155   HEX-LOWER-M
      *   n           6E    156   HEX-LOWER-N
      *   o           6F    157   HEX-LOWER-O
      *   p           70    160   HEX-LOWER-P
      *   q           71    161   HEX-LOWER-Q
      *   r           72    162   HEX-LOWER-R
      *   s           73    163   HEX-LOWER-S
      *   t           74    164   HEX-LOWER-T
      *   u           75    165   HEX-LOWER-U
      *   v           76    166   HEX-LOWER-V
      *   w           77    167   HEX-LOWER-W
      *   x           78    170   HEX-LOWER-X
      *   y           79    171   HEX-LOWER-Y
      *   z           7A    172   HEX-LOWER-Z
      *   {           7B    173   HEX-LEFT-BRACE
      *   |           7C    174   HEX-PIPE
      *                           HEX-PIPE-SIGN
      *   }           7D    175   HEX-RIGHT-BRACE
      *   ~           7E    176   HEX-TILDE
      *   DEL         7F    177   HEX-DEL
      *
      ******************************************************************
       78  HEX-NUL                            VALUE X"00".
       78  HEX-SOH                            VALUE X"01".
       78  HEX-STX                            VALUE X"02".
       78  HEX-ETX                            VALUE X"03".
       78  HEX-EOT                            VALUE X"04".
       78  HEX-ENQ                            VALUE X"05".
       78  HEX-ACK                            VALUE X"06".
       78  HEX-BEL                            VALUE X"07".
       78  HEX-BS                             VALUE X"08".
       78  HEX-HT                             VALUE X"09".
       78  HEX-NL                             VALUE X"0A".
       78  HEX-VT                             VALUE X"0B".
       78  HEX-NP                             VALUE X"0C".
       78  HEX-CR                             VALUE X"0D".
       78  HEX-SO                             VALUE X"0E".
       78  HEX-SI                             VALUE X"0F".
       78  HEX-DLE                            VALUE X"10".
       78  HEX-DC1                            VALUE X"11".
       78  HEX-DC2                            VALUE X"12".
       78  HEX-DC3                            VALUE X"13".
       78  HEX-DC4                            VALUE X"14".
       78  HEX-NAK                            VALUE X"15".
       78  HEX-SYN                            VALUE X"16".
       78  HEX-ETB                            VALUE X"17".
       78  HEX-CAN                            VALUE X"18".
       78  HEX-EM                             VALUE X"19".
       78  HEX-SUB                            VALUE X"1A".
       78  HEX-ESC                            VALUE X"1B".
       78  HEX-FS                             VALUE X"1C".
       78  HEX-GS                             VALUE X"1D".
       78  HEX-RS                             VALUE X"1E".
       78  HEX-US                             VALUE X"1F".
       78  HEX-SP                             VALUE X"20".

      *-----------------------------------------------------------------
       78  HEX-EXCLAMATION                    VALUE X"21".
       78  HEX-DOUBLE-QUOTE                   VALUE X"22".
       78  HEX-NUMBER                         VALUE X"23".
       78  HEX-DOLLAR                         VALUE X"24".
       78  HEX-PERCENT                        VALUE X"25".
       78  HEX-AND                            VALUE X"26".
       78  HEX-SINGLE-QUOTE                   VALUE X"27".
       78  HEX-LEFT-PARENTHESIS               VALUE X"28".
       78  HEX-RIGHT-PARENTHESIS              VALUE X"29".
       78  HEX-ASTERISK                       VALUE X"2A".
       78  HEX-PLUS                           VALUE X"2B".
       78  HEX-COMMA                          VALUE X"2C".
       78  HEX-MINUS                          VALUE X"2D".
       78  HEX-PERIOD                         VALUE X"2E".
       78  HEX-SLASH                          VALUE X"2F".

      *------------------------------------------------------{ NUMBERS }
       78  HEX-0                              VALUE X"30".
       78  HEX-1                              VALUE X"31".
       78  HEX-2                              VALUE X"32".
       78  HEX-3                              VALUE X"33".
       78  HEX-4                              VALUE X"34".
       78  HEX-5                              VALUE X"35".
       78  HEX-6                              VALUE X"36".
       78  HEX-7                              VALUE X"37".
       78  HEX-8                              VALUE X"38".
       78  HEX-9                              VALUE X"39".

      *-----------------------------------------------------------------
       78  HEX-COLON                          VALUE X"3A".
       78  HEX-SEMI-COLON                     VALUE X"3B".
       78  HEX-LESS-THAN                      VALUE X"3C".
       78  HEX-EQUAL                          VALUE X"3D".
       78  HEX-GREATER-THAN                   VALUE X"3E".
       78  HEX-QUESTION                       VALUE X"3F".
       78  HEX-AT                             VALUE X"40".

      *------------------------------------------{ LETTERS: UPPER CASE }
       78  HEX-UPPER-A                        VALUE X"41".
       78  HEX-UPPER-B                        VALUE X"42".
       78  HEX-UPPER-C                        VALUE X"43".
       78  HEX-UPPER-D                        VALUE X"44".
       78  HEX-UPPER-E                        VALUE X"45".
       78  HEX-UPPER-F                        VALUE X"46".
       78  HEX-UPPER-G                        VALUE X"47".
       78  HEX-UPPER-H                        VALUE X"48".
       78  HEX-UPPER-I                        VALUE X"49".
       78  HEX-UPPER-J                        VALUE X"4A".
       78  HEX-UPPER-K                        VALUE X"4B".
       78  HEX-UPPER-L                        VALUE X"4C".
       78  HEX-UPPER-M                        VALUE X"4D".
       78  HEX-UPPER-N                        VALUE X"4E".
       78  HEX-UPPER-O                        VALUE X"4F".
       78  HEX-UPPER-P                        VALUE X"50".
       78  HEX-UPPER-Q                        VALUE X"51".
       78  HEX-UPPER-R                        VALUE X"52".
       78  HEX-UPPER-S                        VALUE X"53".
       78  HEX-UPPER-T                        VALUE X"54".
       78  HEX-UPPER-U                        VALUE X"55".
       78  HEX-UPPER-V                        VALUE X"56".
       78  HEX-UPPER-W                        VALUE X"57".
       78  HEX-UPPER-X                        VALUE X"58".
       78  HEX-UPPER-Y                        VALUE X"59".
       78  HEX-UPPER-Z                        VALUE X"5A".

      *-----------------------------------------------------------------
       78  HEX-LEFT-BRACKET                   VALUE X"5B".
       78  HEX-BACK-SLASH                     VALUE X"5C".
       78  HEX-RIGHT-BRACKET                  VALUE X"5D".
       78  HEX-CARET                          VALUE X"5E".
       78  HEX-UNDER-SCORE                    VALUE X"5F".
      *78  HEX-                               VALUE X"60".

      *------------------------------------------{ LETTERS: LOWER CASE }
       78  HEX-LOWER-A                        VALUE X"61".
       78  HEX-LOWER-B                        VALUE X"62".
       78  HEX-LOWER-C                        VALUE X"63".
       78  HEX-LOWER-D                        VALUE X"64".
       78  HEX-LOWER-E                        VALUE X"65".
       78  HEX-LOWER-F                        VALUE X"66".
       78  HEX-LOWER-G                        VALUE X"67".
       78  HEX-LOWER-H                        VALUE X"68".
       78  HEX-LOWER-I                        VALUE X"69".
       78  HEX-LOWER-J                        VALUE X"6A".
       78  HEX-LOWER-K                        VALUE X"6B".
       78  HEX-LOWER-L                        VALUE X"6C".
       78  HEX-LOWER-M                        VALUE X"6D".
       78  HEX-LOWER-N                        VALUE X"6E".
       78  HEX-LOWER-O                        VALUE X"6F".
       78  HEX-LOWER-P                        VALUE X"70".
       78  HEX-LOWER-Q                        VALUE X"71".
       78  HEX-LOWER-R                        VALUE X"72".
       78  HEX-LOWER-S                        VALUE X"73".
       78  HEX-LOWER-T                        VALUE X"74".
       78  HEX-LOWER-U                        VALUE X"75".
       78  HEX-LOWER-V                        VALUE X"76".
       78  HEX-LOWER-W                        VALUE X"77".
       78  HEX-LOWER-X                        VALUE X"78".
       78  HEX-LOWER-Y                        VALUE X"79".
       78  HEX-LOWER-Z                        VALUE X"7A".

      *-----------------------------------------------------------------
       78  HEX-LEFT-BRACE                     VALUE X"7B".
       78  HEX-PIPE                           VALUE X"7C".
       78  HEX-RIGHT-BRACE                    VALUE X"7D".
       78  HEX-TILDE                          VALUE X"7E".
       78  HEX-DEL                            VALUE X"7F".

      *=================================================================
      *
      *    S P E C I A L - R E D E F I N I T I O N S
      *
      *=================================================================

      *----------------------------------------------------{ AMPERSAND }
       78  HEX-AMPERSAND                      VALUE HEX-AND.
       78  HEX-AND-SIGN                       VALUE HEX-AND.

      *------------------------------------------------------{ AT SIGN }
       78  HEX-AT-SIGN                        VALUE HEX-AT.

      *----------------------------------------------{ CARRIAGE RETURN }
       78  HEX-CARRIAGE-RETURN                 VALUE HEX-CR.

      *--------------------------------------------------{ DASH/HYPHEN }
       78  HEX-DASH                           VALUE HEX-MINUS.
       78  HEX-HYPHEN                         VALUE HEX-MINUS.

      *--------------------------------------------------{ DOLLAR SIGN }
       78  HEX-DOLLAR-SIGN                    VALUE HEX-DOLLAR.

      *-------------------------------------------------------{ ESCAPE }
       78  HEX-EC                             VALUE HEX-ESC.
       78  HEX-ESCAPE                         VALUE HEX-ESC.

      *--------------------------------------------{ EXCLAMATION POINT }
       78  HEX-EXCLAMATION-POINT              VALUE HEX-EXCLAMATION.

      *------------------------------------------------{ FORWARD SLASH }
       78  HEX-FORWARD-SLASH                  VALUE HEX-SLASH.

      *----------------------------------------------------{ LINE FEED }
       78  HEX-LINE-FEED                       VALUE HEX-NL.

      *--------------------------------------------------{ NUMBER SIGN }
       78  HEX-NUMBER-SIGN                    VALUE HEX-NUMBER.

      *-------------------------------------------------{ PERCENT SIGN }
       78  HEX-PERCENT-SIGN                   VALUE HEX-PERCENT.

      *----------------------------------------------------{ PIPE SIGN }
       78  HEX-PIPE-SIGN                      VALUE HEX-PIPE.

      *------------------------------------------------{ QUESTION MARK }
       78  HEX-QUESTION-MARK                  VALUE HEX-QUESTION.

      *--------------------------------------------------------{ SPACE }
       78  HEX-SPACE                          VALUE HEX-SP.

      *-----------------------------------------------{ HORIZONTAL TAB }
       78  HEX-TAB                             VALUE HEX-HT.

