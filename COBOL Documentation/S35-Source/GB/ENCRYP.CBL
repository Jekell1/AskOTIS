       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCRYP.
      *********************************************************
      * VERYANT
      *
      * PASSWORD ENCRYPT/DEENCRYPT UTILITY
      * CONVERT FROM C ROUTINE
      *
      * REV:
      * MJD 130510 NEW PROGRAM FROM VERYANT (PASSWD.CBL).
      *            ADDED "LIB__/" TO COPY STATEMENTS.
      *            CHANGED ALL "BW-" TO BE "BWARGS-" TO AVOID CONFUSION
      *            WITH OUR BW-REC FIELDS
      *            ADDED GETFMW, NEEDED FORM PATH TO CALL "GB/BITWIS"
      *********************************************************

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/ENCRYP A32 08/02/19-06:44:10 >".
       COPY "LIBGB/BWARGS.CPY".

       78  MASTER-PASSWORD            VALUE "BACKDOOR".

       78  HUNDRED-BILLION            VALUE 100000000000.
       77  WS-TIME1                   PIC 9(8).
       01  MASTER-PASSWORD-LOWER      PIC X(8).
       01  RAND-NUM                   PIC 9(10).

       77 SK-I       USAGE SIGNED-INT.
       77 SK-J       USAGE SIGNED-INT.
       77 SK-K       USAGE SIGNED-INT.
       77 SK-T       USAGE SIGNED-INT.
       01 SK-KEY-FULL.
          05 SK-KEY     PIC X COMP-X OCCURS 66 TIMES.
       01 C          PIC X COMP-X OCCURS 28 TIMES.
       01 D          PIC X COMP-X OCCURS 28 TIMES.

       77 WS-CHAR    PIC X.
       77 WS-C       PIC X COMP-X.
       77 WS-I       USAGE SIGNED-INT.
       77 WS-T       USAGE SIGNED-INT.
       77 WS-TT      USAGE SIGNED-INT.
       77 WS-J       USAGE SIGNED-INT.
       77 WS-K       USAGE SIGNED-INT.
       77 WS-IDX     PIC S9(5).
       77 WS-IDX2    PIC S9(5).
       77 WS-NUM     PIC X COMP-X.

       01 SALT       USAGE SIGNED-INT.
       01 SALTC-FULL.
          05 SALTC      PIC X COMP-X OCCURS 3 TIMES.

       01 CRY-SALT-FULL.
          05 CRY-SALT   PIC X COMP-X OCCURS 3 TIMES.
       77 CRY-C      USAGE SIGNED-INT.
       77 CRY-I      USAGE SIGNED-INT.
       77 CRY-J      USAGE SIGNED-INT.
       77 CRY-K      USAGE SIGNED-INT.
       77 CRY-M      USAGE SIGNED-INT.
       77 CRY-TEMP   USAGE SIGNED-INT.
       01 CRY-BLOCK-FULL.
          05 CRY-BLOCK  PIC X COMP-X OCCURS 66 TIMES.
       77 CRY-IOBUF  PIC X COMP-X OCCURS 16 TIMES.
       01 IOBUF-FULL.
          05 IOBUF      PIC X(16).

       77 CRY-PP     PIC X COMP-X OCCURS 10 TIMES.

       77 ORD-Z      PIC X COMP-X.
       77 ORD-9      PIC X COMP-X.
       77 ORD-PERIOD PIC X COMP-X.

       01 ENC-BLOCK-FULL.
          05 ENC-BLOCK  PIC X COMP-X OCCURS 66 TIMES.
       01 L          PIC X COMP-X OCCURS 32 TIMES.
       01 R          PIC X COMP-X OCCURS 32 TIMES.
       01 TEMPL      PIC X COMP-X OCCURS 32 TIMES.
       01 F          PIC X COMP-X OCCURS 32 TIMES.
       01 PRES       PIC X COMP-X OCCURS 48 TIMES.
       01 E          PIC X COMP-X OCCURS 48 TIMES.
       01 FILLER     OCCURS 16 TIMES.
          05 FILLER  OCCURS 48 TIMES.
             10 KS   PIC X COMP-X.

       01 FILLER.
          05 IP-DATA PIC X(128) VALUE
	     "5850423426181002605244362820120462544638302214066456484032
      -      "2416085749413325170901595143352719110361534537292113056355
      -      "473931231507".
          05 IP-OCCURS REDEFINES IP-DATA.
             10 IP OCCURS 64 TIMES PIC 9(2).

       01 FILLER.
          05 FP-DATA PIC X(128) VALUE
                                       "40084816562464323907471555236331
      -                                "38064614542262303705451353216129
      -                                "36044412522060283503431151195927
      -                              "34024210501858263301410949175725".
          05 FP-OCCURS REDEFINES FP-DATA.
             10 FP OCCURS 64 TIMES PIC 9(2).

       01 FILLER.
          05 P-DATA PIC X(64) VALUE    "16072021291228170115232605183110
      -                              "02082414322703091913300622110425".
          05 P-OCCURS REDEFINES P-DATA.
             10 P OCCURS 32 TIMES PIC 9(2).

       01 FILLER.
          05 S-DATA PIC X(1024) VALUE  "14041301021511080310061205090007
      -                                "00150704140213011006121109050308
      -                                "04011408130602111512090703100500
      -                                "15120802040901070511031410000613
      -                                "15010814061103040907021312000510
      -                                "03130407150208141200011006091105
      -                                "00140711100413010508120609030215
      -                                "13081001031504021106071200051409
      -                                "10000914060315050113120711040208
      -                                "13070009030406100208051412111501
      -                                "13060409081503001101021205101407
      -                                "01101300060908070415140311050212
      -                                "07131403000609100102080511120415
      -                                "13081105061500030407021201101409
      -                                "10060900121107131501031405020804
      -                                "03150006100113080904051112070214
      -                                "02120401071011060805031513001409
      -                                "14110212040713010500151003090806
      -                                "04020111101307081509120506030014
      -                                "11081207011402130615000910040503
      -                                "12011015090206080013030414070511
      -                                "10150402071209050601131400110308
      -                                "09141505020812030700041001131106
      -                                "04030212090515101114010706000813
      -                                "04110214150008130312090705100601
      -                                "13001107040901101403051202150806
      -                                "01041113120307141015060800050902
      -                                "06111308010410070905001514020312
      -                                "13020804061511011009031405001207
      -                                "01151308100307041205061100140902
      -                                "07110401091214020006101315030508
      -                              "02011407041008131512090003050611".
          05 S-OCCURS REDEFINES S-DATA.
             10 FILLER OCCURS 8 TIMES.
                15 FILLER OCCURS 64 TIMES.
                   20 S   PIC 9(2).

       01 FILLER.
          05 PC1-C-DATA PIC X(56) VALUE  "574941332517090158504234261810
      -                                  "02595143352719110360524436".

          05 PC1-C-OCCURS REDEFINES PC1-C-DATA.
             10 PC1-C OCCURS 28 TIMES PIC 9(2).

       01 FILLER.
          05 PC1-D-DATA PIC X(56) VALUE  "635547393123150762544638302214
      -                                  "06615345372921130528201204".

          05 PC1-D-OCCURS REDEFINES PC1-D-DATA.
             10 PC1-D OCCURS 28 TIMES PIC 9(2).

       01 FILLER.
          05 PC2-C-DATA PIC X(48) VALUE  "141711240105032815062110231912
      -                                  "042608160727201302".
          05 PC2-C-OCCURS REDEFINES PC2-C-DATA.
             10 PC2-C OCCURS 24 TIMES PIC 9(2).

       01 FILLER.
          05 PC2-D-DATA PIC X(48) VALUE  "415231374755304051453348444939
      -                                  "563453464250362932".
          05 PC2-D-OCCURS REDEFINES PC2-D-DATA.
             10 PC2-D OCCURS 24 TIMES PIC 9(2).

       01 FILLER.
          05 E2-DATA PIC X(96) VALUE     "320102030405040506070809080910
      -                                  "111213121314151617161718192021
      -                                  "202122232425242526272829282930
      -                                  "313201".
          05 E2-OCCURS REDEFINES E2-DATA.
             10 E2 OCCURS 48 TIMES PIC 9(2).

       01 FILLER.
          05 SHIFTS-DATA PIC X(96) VALUE "320102030405040506070809080910
      -                                  "111213121314151617161718192021
      -                                  "202122232425242526272829282930
      -                                  "313201".
          05 SHIFTS-OCCURS REDEFINES SHIFTS-DATA.
             10 SHIFTS OCCURS 48 TIMES PIC 9(2).

       01  MESS  PIC X.
      *LINKAGE SECTION.
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGB/PASSARGS.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME
                                PASSARGS 
                                EXIT-PATHNAME. 
       MAIN.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE MASTER-PASSWORD TO MASTER-PASSWORD-LOWER.
           CALL "C$TOLOWER" USING MASTER-PASSWORD-LOWER, VALUE 8.
           IF (PA-PP = MASTER-PASSWORD OR PA-PP = MASTER-PASSWORD-LOWER)
              MOVE "00" TO PA-STAT
           ELSE
              PERFORM DO-PASSWD
              IF PA-ENCRYPTED = SPACES
                   MOVE "00" TO PA-STAT
                   MOVE IOBUF TO PA-ENCRYPTED
              ELSE
                IF IOBUF = PA-ENCRYPTED
                   MOVE "00" TO PA-STAT
                ELSE
                   MOVE "01" TO PA-STAT
                END-IF
              END-IF
           END-IF
           GOBACK
           .

       TIME-STAMP.
           ACCEPT WS-TIME1 FROM TIME
           DISPLAY WS-TIME1
           .

       DO-PASSWD.
           PERFORM CONVERT-PASSWD
           PERFORM GET-ORDS
           PERFORM GET-SALT
           PERFORM SET-SALTC

           MOVE SALTC-FULL TO CRY-SALT-FULL
           PERFORM CRYPT
           MOVE CRY-SALT-FULL TO SALTC-FULL
           PERFORM UNCONVERT-PASSWD
           .

       SET-SALTC.
           MOVE FUNCTION ORD('X') TO SALTC(1)
           MOVE FUNCTION ORD('Y') TO SALTC(2)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I NOT < 3
             COMPUTE WS-C = SALTC(WS-I) + ORD-PERIOD
             IF WS-C > ORD-9 ADD 7 TO WS-C END-IF
             IF WS-C > ORD-Z ADD 6 TO WS-C END-IF
             MOVE WS-C TO SALTC(WS-I)
           END-PERFORM
           .

       CONVERT-PASSWD.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
             MOVE PA-PP(WS-I:1) TO WS-CHAR
             IF WS-CHAR = SPACE
                MOVE ZERO TO CRY-PP(WS-I)
             ELSE
                MOVE FUNCTION ORD(WS-CHAR) TO CRY-PP(WS-I)
             END-IF
           END-PERFORM
           .

       UNCONVERT-PASSWD.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
             MOVE CRY-IOBUF(WS-I) TO WS-C
             IF WS-C NOT = 0
                MOVE FUNCTION CHAR (WS-C) TO WS-CHAR
                MOVE WS-CHAR TO IOBUF(WS-I:1)
             ELSE
                MOVE SPACE TO IOBUF(WS-I:1)
             END-IF
           END-PERFORM
           CONTINUE
           .

       GET-SALT.
           ACCEPT WS-TIME1 FROM TIME
           COMPUTE RAND-NUM = HUNDRED-BILLION *
                   (1 / FUNCTION RANDOM(WS-TIME1) )
           MOVE RAND-NUM TO SALT
           .


       ENCRYPT.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL  WS-J NOT < 33
             COMPUTE WS-IDX = IP(WS-J) - 1
             IF WS-IDX < 1 MOVE 1 TO WS-IDX END-IF
             MOVE ENC-BLOCK( WS-IDX ) TO L(WS-J)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I NOT < 17

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 33
               MOVE R(WS-J) TO TEMPL(WS-J)
             END-PERFORM

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 49
               COMPUTE WS-IDX = E(WS-J) - 1
               IF WS-IDX < 1 MOVE 1 TO WS-IDX END-IF
               IF WS-IDX > 32 MOVE 32 TO WS-IDX END-IF
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-XOR        TO TRUE
               SET  BWARGS-TINT       TO TRUE
               MOVE R(WS-IDX)     TO BWARGS-OP1-INT
               MOVE KS(WS-I,WS-J) TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT    TO PRES(WS-J)
             END-PERFORM

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 9
               COMPUTE WS-T = ( 6 * ( WS-J - 1 ) ) + 1
               MOVE ZERO TO WS-IDX

               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-LEFTSHIFT  TO TRUE
               MOVE WS-T TO WS-TT
               MOVE PRES(WS-TT)   TO BWARGS-OP1-INT
               MOVE 5             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-IDX

               COMPUTE WS-TT = WS-T + 1

               MOVE PRES(WS-TT)   TO BWARGS-OP1-INT
               MOVE 3             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-IDX

               COMPUTE WS-TT = WS-T + 2
               MOVE PRES(WS-TT)   TO BWARGS-OP1-INT
               MOVE 2             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-IDX

               COMPUTE WS-TT = WS-T + 3
               MOVE PRES(WS-TT)   TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-IDX

               COMPUTE WS-TT = WS-T + 4
               ADD PRES(WS-TT)    TO WS-IDX

               COMPUTE WS-TT = WS-T + 5
               MOVE PRES(WS-TT)   TO BWARGS-OP1-INT
               MOVE 4             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-IDX

               IF WS-IDX < 1  MOVE  1 TO WS-IDX END-IF
               IF WS-IDX > 64 MOVE 64 TO WS-IDX END-IF
               MOVE S(WS-J,WS-IDX) TO WS-K

               COMPUTE WS-T = ( 4 * ( WS-J - 1 ) ) + 1

               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-RIGHTSHIFT TO TRUE
               MOVE WS-K          TO BWARGS-OP1-INT
               MOVE 3             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-NUM
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-AND        TO TRUE
               MOVE WS-NUM        TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE WS-T TO WS-TT
               MOVE  BWARGS-RES-INT   TO F(WS-TT)

               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-RIGHTSHIFT TO TRUE
               MOVE WS-K          TO BWARGS-OP1-INT
               MOVE 2             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-NUM
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-AND        TO TRUE
               MOVE WS-NUM        TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               COMPUTE WS-TT = WS-T + 1
               MOVE  BWARGS-RES-INT   TO F(WS-TT)

               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-RIGHTSHIFT TO TRUE
               MOVE WS-K          TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               ADD  BWARGS-RES-INT    TO WS-NUM
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-AND        TO TRUE
               MOVE WS-NUM        TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               COMPUTE WS-TT = WS-T + 2
               MOVE  BWARGS-RES-INT   TO F(WS-TT)

               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-AND        TO TRUE
               MOVE WS-K          TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               COMPUTE WS-TT = WS-T + 3
               MOVE  BWARGS-RES-INT   TO F(WS-TT)

             END-PERFORM

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 33
                COMPUTE WS-IDX = P(WS-J) - 1
               IF WS-IDX < 1  MOVE  1 TO WS-IDX END-IF
               IF WS-IDX > 32 MOVE 32 TO WS-IDX END-IF
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-XOR        TO TRUE
               SET  BWARGS-TINT       TO TRUE
               MOVE F(WS-IDX)     TO BWARGS-OP1-INT
               MOVE L(WS-J)       TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT    TO R(WS-J)
             END-PERFORM

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 33
               MOVE TEMPL(WS-J) TO L(WS-J)
             END-PERFORM

           END-PERFORM

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 33
             MOVE L(WS-J) TO WS-T
             MOVE R(WS-J) TO L(WS-J)
             MOVE WS-T    TO R(WS-J)
           END-PERFORM

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J NOT < 65
             COMPUTE WS-IDX = FP(WS-J) - 1
             IF WS-IDX < 1  MOVE  1 TO WS-IDX END-IF
             IF WS-IDX > 32 MOVE 32 TO WS-IDX END-IF
             MOVE L(WS-IDX) TO ENC-BLOCK(WS-J)
           END-PERFORM
           .


       CRYPT.
           PERFORM VARYING CRY-I FROM 1 BY 1 UNTIL CRY-I NOT < 67
               MOVE ZERO TO CRY-BLOCK(CRY-I)
               IF CRY-I < 17
                  MOVE ZERO TO CRY-IOBUF(CRY-I)
               END-IF
           END-PERFORM

           MOVE 1 TO CRY-M
           PERFORM VARYING CRY-I FROM 1 BY 1
                                 UNTIL (CRY-I NOT < 65) OR
                                       (CRY-PP(CRY-M) = 0)
             MOVE CRY-PP(CRY-M) TO CRY-C
             PERFORM VARYING CRY-J FROM 1 BY 1 UNTIL CRY-J NOT < 8
               COMPUTE WS-IDX = 6 - CRY-J
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-RIGHTSHIFT TO TRUE
               MOVE CRY-C         TO BWARGS-OP1-INT
               MOVE WS-IDX        TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT    TO WS-NUM

               SET  BWARGS-AND TO TRUE
               MOVE WS-NUM TO BWARGS-OP1-INT
               MOVE 1      TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT TO CRY-BLOCK(CRY-I)
               ADD 1 TO CRY-I
             END-PERFORM
             ADD 1 TO CRY-M
           END-PERFORM

           MOVE CRY-BLOCK-FULL TO SK-KEY-FULL
           PERFORM SETKEY
           MOVE SK-KEY-FULL TO CRY-BLOCK-FULL

           PERFORM VARYING CRY-I FROM 1 BY 1 UNTIL CRY-I NOT < 67
             MOVE ZERO TO CRY-BLOCK(CRY-I)
           END-PERFORM

           PERFORM VARYING CRY-I FROM 1 BY 1 UNTIL CRY-I NOT < 3
             MOVE CRY-SALT(CRY-I) TO CRY-C
             MOVE CRY-C TO CRY-IOBUF(CRY-I)
             IF CRY-C > ORD-Z ADD -6 TO CRY-C END-IF
             IF CRY-C > ORD-9 ADD -7 TO CRY-C END-IF
             COMPUTE CRY-C = CRY-C - ORD-PERIOD
             PERFORM VARYING CRY-J FROM 1 BY 1 UNTIL CRY-J NOT < 7
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-RIGHTSHIFT TO TRUE
               MOVE CRY-C         TO BWARGS-OP1-INT
               MOVE CRY-J         TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT    TO WS-NUM

               SET  BWARGS-AND TO TRUE
               MOVE WS-NUM TO BWARGS-OP1-INT
               MOVE 1      TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT TO WS-NUM

               IF (WS-NUM NOT = ZERO)
                  COMPUTE WS-IDX  = (6 * CRY-I) + CRY-J
                  COMPUTE WS-IDX2 = (6 * CRY-I) + CRY-J + 24
                  MOVE E(WS-IDX)  TO CRY-TEMP
                  MOVE E(WS-IDX2) TO E(WS-IDX)
                  MOVE CRY-TEMP   TO E(WS-IDX2)
               END-IF
             END-PERFORM
           END-PERFORM

           MOVE CRY-BLOCK-FULL TO ENC-BLOCK-FULL
           PERFORM ENCRYPT
           MOVE ENC-BLOCK-FULL TO CRY-BLOCK-FULL

           PERFORM VARYING CRY-I FROM 1 BY 1 UNTIL CRY-I NOT < 12
             MOVE ZERO TO CRY-C
             PERFORM VARYING CRY-J FROM 1 BY 1 UNTIL CRY-J NOT < 7
               INITIALIZE BWARGS-ARGS
               SET  BWARGS-TINT       TO TRUE
               SET  BWARGS-LEFTSHIFT  TO TRUE
               MOVE CRY-C         TO BWARGS-OP1-INT
               MOVE 1             TO BWARGS-POSITIONS
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT    TO WS-NUM

               COMPUTE WS-IDX = (6 * ( CRY-I - 1 )) + CRY-J
               IF WS-IDX > 66 MOVE 66 TO WS-IDX END-IF
               SET BWARGS-OR             TO TRUE
               MOVE WS-NUM           TO BWARGS-OP1-INT
               MOVE CRY-BLOCK(WS-IDX) TO BWARGS-OP2-INT
               MOVE "GB/BITWIS" TO FORM-NAM
               CALL FORM-PROGX USING BWARGS-ARGS
               CANCEL FORM-PROGX
      *------->CALL "BITWISEUTL" USING BWARGS-ARGS
               MOVE BWARGS-RES-INT       TO CRY-C
             END-PERFORM
             ADD ORD-PERIOD TO CRY-C
             IF CRY-C > ORD-9 ADD 7 TO CRY-C END-IF
             IF CRY-C > ORD-Z ADD 6 TO CRY-C END-IF
             COMPUTE WS-IDX = CRY-I + 2
             MOVE CRY-C TO CRY-IOBUF(WS-IDX)
           END-PERFORM
           COMPUTE WS-IDX = CRY-I + 2
           MOVE ZERO TO CRY-IOBUF(WS-IDX)
           IF CRY-IOBUF(2) = ZERO
              MOVE CRY-IOBUF(1) TO CRY-IOBUF(2)
           END-IF

           PERFORM VARYING CRY-I FROM 1 BY 1 UNTIL CRY-I > 16
             IF CRY-IOBUF(CRY-I) = ZERO
                EXIT PERFORM
             END-IF
             COMPUTE CRY-IOBUF(CRY-I) = CRY-IOBUF(CRY-I) + CRY-I
             IF CRY-IOBUF(CRY-I) < 49
                COMPUTE CRY-IOBUF(CRY-I) = ( CRY-IOBUF(CRY-I) / 3 ) + 70
             END-IF
             IF CRY-IOBUF(CRY-I) > 91 AND CRY-IOBUF(CRY-I) < 98
                ADD -6 TO CRY-IOBUF(CRY-I)
             END-IF
             IF CRY-IOBUF(CRY-I) > 123
                COMPUTE CRY-IOBUF(CRY-I) = 123 -
                        ( ( CRY-IOBUF(CRY-I) - 123 ) / 10 )
             END-IF
           END-PERFORM
           CONTINUE
           .

       SETKEY.
           PERFORM VARYING SK-I FROM 1 BY 1 UNTIL SK-I NOT < 29
             COMPUTE WS-IDX  = PC1-C(SK-I) - 1     + 1
             COMPUTE WS-IDX2 = PC1-D(SK-I) - 1     + 1
             MOVE SK-KEY(WS-IDX)  TO C(SK-I)
             MOVE SK-KEY(WS-IDX2) TO D(SK-I)
           END-PERFORM

           PERFORM VARYING SK-I FROM 1 BY 1 UNTIL SK-I NOT < 17
             CONTINUE
             PERFORM VARYING SK-K FROM 1 BY 1 UNTIL
                     SK-K NOT < SHIFTS(SK-I)
               MOVE C(1) TO SK-T
               PERFORM VARYING SK-J FROM 1 BY 1 UNTIL SK-J NOT < 28
                 COMPUTE WS-IDX = SK-J + 1
                 MOVE C(WS-IDX) TO C(SK-J)
               END-PERFORM
               MOVE SK-T TO C(28)
               MOVE D(1) TO SK-T
               PERFORM VARYING SK-J FROM 1 BY 1 UNTIL SK-J NOT < 28
                 COMPUTE WS-IDX = SK-J + 1
                 MOVE D(WS-IDX) TO D(SK-J)
               END-PERFORM
               MOVE SK-T TO D(28)
             END-PERFORM
             PERFORM VARYING SK-J FROM 1 BY 1 UNTIL SK-J NOT < 25
                COMPUTE WS-IDX = PC2-C(SK-J) - 1    + 1
                MOVE C(WS-IDX)  TO KS(SK-I,SK-J)
                COMPUTE WS-IDX  = PC2-D(SK-J) - 28 - 1  + 1
                COMPUTE WS-IDX2 = SK-J + 24
                MOVE D(WS-IDX)  TO KS(SK-I,WS-IDX2)
             END-PERFORM
           END-PERFORM

           PERFORM VARYING SK-I FROM 1 BY 1 UNTIL SK-I NOT < 49
             MOVE E2(SK-I) TO E(SK-I)
           END-PERFORM
           .

       GET-ORDS.
           MOVE FUNCTION ORD("Z") TO ORD-Z
           MOVE FUNCTION ORD("9") TO ORD-9
           MOVE FUNCTION ORD(".") TO ORD-PERIOD
           .

