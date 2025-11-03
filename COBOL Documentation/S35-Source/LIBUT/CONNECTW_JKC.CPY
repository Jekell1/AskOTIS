      * COPYMEMBER: LIBGB/CONNECTW_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/CONNECTW_SQL
      * DESCRIPTION   : SQL SERVER CONNECTION WORKERS
      *
      *=================================================================
      * REV:
      * JKC 2021-0115 ***NEW***
      * JKC 2021-0223 CLEANING UP THE CODE.
      * JKC 2021-0324 ADDED SQL-DATE-YYYY-MM-DD AND SQL-DATE-YYYYMMDD.
      * JKC 2021-0430 CHANGES MADE WHILE WORKING ON DECLARE ROUTINES.
      * JKC 2021-0511 CHANGED NAME FROM SQLCONNECTW TO CONNECTW_SQL
      * JKC 2021-0525 UPDATED FROM XPC/IOCONN.PRE FOR LATEST CHANGES.
      * JKC 2021-0817 UPDATED FROM XPC/IOCONN.PRE FOR LATEST CHANGES.
      * JKC 2022-0209 DID SOME CLEANUP OF UNNEED CODE THAT WAS USED
      *               FOR A TWO-STEP COMPILE PROCESS; WE USE A ONE-STEP
      *               COMPILE GOING FORWARD AT THIS POINT.
      * JKC 2022-0622 MOVED THIS FROM LIBIO TO LIBGB; LIBIO WAS REMOVED
      * JKC 2022-0805 CHANGED SQL-GP-KEY TO BE SQL-CONNECT-VERIFY;
      *               MOVED SQL-CONNECT-WORKERS WITHIN
      *               'EXEC SQL BEGIN DECLARE SECTION END-EXEC.'
      * JKC 2022-0805 ADDED SQL-TEXT-SIZE.
      * JKC 2022-1003 ADDED SQL-TEXT-SIZE.
      * JKC 2022-1010 ADDED SQL-TEXT-S3.
      * JKC 2023-0815 ADDED SQL-TEXT-ASCII; HAS ALL HEX VALUES OF
      *          ALL CHARACTERS (ALPHA, NUMBERS, SPECIAL); THERE WAS AN
      *          ISSUE IN THE TEXT INCREMENT ROUTINE WHERE A "60+" WAS
      *          BEING INCREMENTED TO "60 " WHICH RESETS FROM THE START
      *          GIVING BAD RESULTS IN SCAN WINDOW; NEEDED TO ALLOW THE
      *          USE OF SPECIAL CHARACTERS TO INCREMENT OR DECREMENT.
      *
      ******************************************************************
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-CONNECT-WORKERS.
           03  SQL-CONNECT-SERVER          PIC X(30).
           03  SQL-CONNECT-USERID          PIC X(30).
           03  SQL-CONNECT-PSWD            PIC X(30).
           03  SQL-CONNECT-VERIFY          PIC 99.
      *??? 03  SQL-CONNECT-DATABASE        PIC X(20).
      *??? 03  SQL-CONNECT-OWNER           PIC X(20).
      *??? 03  SQL-CONNECT-PRE-FILE        PIC X(20).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  SQL-DATE-WORKERS.
           03  SQL-DATE-YYYYMMDD                     PIC 9(8).
           03  FILLER REDEFINES SQL-DATE-YYYYMMDD.
               05  SQL-DATE-YYYYMMDD-YYYY            PIC 9(4).
               05  SQL-DATE-YYYYMMDD-MM              PIC 9(2).
               05  SQL-DATE-YYYYMMDD-DD              PIC 9(2).

           03  SQL-DATE-YYYY-MM-DD                   PIC X(10).
           03  SQL-DATE-YYYY-MM-DD-NUM REDEFINES SQL-DATE-YYYY-MM-DD
                                                     PIC 9999/99/99.
           03  FILLER REDEFINES SQL-DATE-YYYY-MM-DD.
               05  SQL-DATE-YYYY-MM-DD-YYYY          PIC 9(4).
               05  FILLER                            PIC X(1).
               05  SQL-DATE-YYYY-MM-DD-MM            PIC 9(2).
               05  FILLER                            PIC X(1).
               05  SQL-DATE-YYYY-MM-DD-DD            PIC 9(2).


       01  SQL-ERROR-BUF.
           03  SQL-ERROR-01                          PIC X(70).
           03  SQL-ERROR-02                          PIC X(70).
           03  SQL-ERROR-03                          PIC X(70).
           03  SQL-ERROR-04                          PIC X(70).
           03  SQL-ERROR-05                          PIC X(70).
           03  FILLER REDEFINES SQL-ERROR-05.
               05  SQL-ERROR-05-SQLCODEX             PIC X(6).
               05  SQL-ERROR-05-SQLCODE              PIC Z(8)9-.
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLSTATEX            PIC X(7).
               05  SQL-ERROR-05-SQLSTATE             PIC X(5).
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLERRMLX            PIC X(7).
               05  SQL-ERROR-05-SQLERRML             PIC 9(4)-.
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLERRPLX            PIC X(7).
               05  SQL-ERROR-05-SQLERRPL             PIC 9(4)-.
           03  SQL-ERROR-06                          PIC X(70).
           03  FILLER REDEFINES SQL-ERROR-06.
               05  SQL-ERROR-06-KEYX                 PIC X(6).
               05  SQL-ERROR-06-KEY                  PIC X(64).
           03  SQL-ERROR-07                          PIC X(70).
           03  SQL-ERROR-08                          PIC X(70).
       01  SQL-DEBUG-BUF REDEFINES SQL-ERROR-BUF.
           03  SQL-DEBUG-01                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-01.
               05  SQL-DEBUG-01-SQLCODEX             PIC X(7).
               05  SQL-DEBUG-01-SQLCODE              PIC 9(9)-.
               05  FILLER                            PIC X(3).
               05  SQL-DEBUG-01-SQLCABCX             PIC X(7).
               05  SQL-DEBUG-01-SQLCABC              PIC 9(9)-.
               05  FILLER                            PIC X(12).
               05  SQL-DEBUG-01-SQLCAIDX             PIC X(6).
               05  SQL-DEBUG-01-SQLCAID              PIC X(8).
           03  SQL-DEBUG-02                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-02.
               05  SQL-DEBUG-02-SQLSTATEX            PIC X(7).
               05  SQL-DEBUG-02-SQLSTATE             PIC X(5).
               05  FILLER                            PIC X(8).
               05  SQL-DEBUG-02-SQLERRMLX            PIC X(7).
               05  SQL-DEBUG-02-SQLERRML             PIC 9(4)-.
               05  FILLER                            PIC X(3).
               05  SQL-DEBUG-02-SQLERRPLX            PIC X(7).
               05  SQL-DEBUG-02-SQLERRPL             PIC 9(4)-.
               05  FILLER                            PIC X(2).
               05  SQL-DEBUG-02-SQLWARNX             PIC X(6).
               05  SQL-DEBUG-02-SQLWARN0             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN1             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN2             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN3             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN4             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN5             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN6             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN7             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN8             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN9             PIC X(1).
               05  SQL-DEBUG-02-SQLWARNA             PIC X(1).
           03  SQL-DEBUG-03                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-03.
               05  SQL-DEBUG-03-SQLERRDX             PIC X(7).
               05  SQL-DEBUG-03-SQLERRD-1            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-03-SQLERRD-2            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-03-SQLERRD-3            PIC 9(9)-.
           03  SQL-DEBUG-04                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-04.
               05  SQL-DEBUG-04-SQLERRDX             PIC X(7).
               05  SQL-DEBUG-04-SQLERRD-4            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-04-SQLERRD-5            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-04-SQLERRD-6            PIC 9(9)-.
           03  SQL-DEBUG-05                          PIC X(70).
           03  SQL-DEBUG-06                          PIC X(70).
           03  SQL-DEBUG-07                          PIC X(70).
           03  SQL-DEBUG-08                          PIC X(70).

      *=================================================================
       77  SQL-TEXT-LAST               PIC 9(02)     VALUE ZEROES.
       77  SQL-TEXT-S1                 PIC S9(2)     VALUE ZEROES.
       77  SQL-TEXT-S2                 PIC S9(2)     VALUE ZEROES.
       77  SQL-TEXT-S3                 PIC S9(2)     VALUE ZEROES.
       77  SQL-TEXT-S4                 PIC S9(2)     VALUE ZEROES.
       77  SQL-TEXT-SIZE               PIC 9(02)     VALUE ZEROES.
       78  SQL-TEXT-ASCII-MAX                        VALUE 95.
       78  SQL-TEXT-MAX                              VALUE 35.

       01  SQL-TEXT-WORKERS.
           03  SQL-TEXT                PIC X(35).
           03  SQL-TEXT9               REDEFINES SQL-TEXT
                                       PIC 9(01) OCCURS SQL-TEXT-MAX.
           03  SQL-TEXTX               REDEFINES SQL-TEXT
                                       PIC X(01) OCCURS SQL-TEXT-MAX.
           03  SQL-TEXT-ASCII.
               05  FILLER              PIC X(1) VALUE X"20".  *> SP
               05  FILLER              PIC X(1) VALUE X"21".  *> !
               05  FILLER              PIC X(1) VALUE X"22".  *> "
               05  FILLER              PIC X(1) VALUE X"23".  *> #
               05  FILLER              PIC X(1) VALUE X"24".  *> $
               05  FILLER              PIC X(1) VALUE X"25".  *> %
               05  FILLER              PIC X(1) VALUE X"26".  *> &
               05  FILLER              PIC X(1) VALUE X"27".  *> '
               05  FILLER              PIC X(1) VALUE X"28".  *> (
               05  FILLER              PIC X(1) VALUE X"29".  *> )
               05  FILLER              PIC X(1) VALUE X"2A".  *> *
               05  FILLER              PIC X(1) VALUE X"2B".  *> +
               05  FILLER              PIC X(1) VALUE X"2C".  *> ,
               05  FILLER              PIC X(1) VALUE X"2D".  *> -
               05  FILLER              PIC X(1) VALUE X"2E".  *> .
               05  FILLER              PIC X(1) VALUE X"2F".  *> /
               05  FILLER              PIC X(1) VALUE X"30".  *> 0
               05  FILLER              PIC X(1) VALUE X"31".  *> 1
               05  FILLER              PIC X(1) VALUE X"32".  *> 2
               05  FILLER              PIC X(1) VALUE X"33".  *> 3
               05  FILLER              PIC X(1) VALUE X"34".  *> 4
               05  FILLER              PIC X(1) VALUE X"35".  *> 5
               05  FILLER              PIC X(1) VALUE X"36".  *> 6
               05  FILLER              PIC X(1) VALUE X"37".  *> 7
               05  FILLER              PIC X(1) VALUE X"38".  *> 8
               05  FILLER              PIC X(1) VALUE X"39".  *> 9
               05  FILLER              PIC X(1) VALUE X"3A".  *> :
               05  FILLER              PIC X(1) VALUE X"3B".  *> ;
               05  FILLER              PIC X(1) VALUE X"3C".  *> <
               05  FILLER              PIC X(1) VALUE X"3D".  *> =
               05  FILLER              PIC X(1) VALUE X"3E".  *> >
               05  FILLER              PIC X(1) VALUE X"3F".  *> ?
               05  FILLER              PIC X(1) VALUE X"40".  *> @
               05  FILLER              PIC X(1) VALUE X"41".  *> A (UP)
               05  FILLER              PIC X(1) VALUE X"42".  *> B (UP)
               05  FILLER              PIC X(1) VALUE X"43".  *> C (UP)
               05  FILLER              PIC X(1) VALUE X"44".  *> D (UP)
               05  FILLER              PIC X(1) VALUE X"45".  *> E (UP)
               05  FILLER              PIC X(1) VALUE X"46".  *> F (UP)
               05  FILLER              PIC X(1) VALUE X"47".  *> G (UP)
               05  FILLER              PIC X(1) VALUE X"48".  *> H (UP)
               05  FILLER              PIC X(1) VALUE X"49".  *> I (UP)
               05  FILLER              PIC X(1) VALUE X"4A".  *> J (UP)
               05  FILLER              PIC X(1) VALUE X"4B".  *> K (UP)
               05  FILLER              PIC X(1) VALUE X"4C".  *> L (UP)
               05  FILLER              PIC X(1) VALUE X"4D".  *> M (UP)
               05  FILLER              PIC X(1) VALUE X"4E".  *> N (UP)
               05  FILLER              PIC X(1) VALUE X"4F".  *> O (UP)
               05  FILLER              PIC X(1) VALUE X"50".  *> P (UP)
               05  FILLER              PIC X(1) VALUE X"51".  *> Q (UP)
               05  FILLER              PIC X(1) VALUE X"52".  *> R (UP)
               05  FILLER              PIC X(1) VALUE X"53".  *> S (UP)
               05  FILLER              PIC X(1) VALUE X"54".  *> T (UP)
               05  FILLER              PIC X(1) VALUE X"55".  *> U (UP)
               05  FILLER              PIC X(1) VALUE X"56".  *> V (UP)
               05  FILLER              PIC X(1) VALUE X"57".  *> W (UP)
               05  FILLER              PIC X(1) VALUE X"58".  *> X (UP)
               05  FILLER              PIC X(1) VALUE X"59".  *> Y (UP)
               05  FILLER              PIC X(1) VALUE X"5A".  *> Z (UP)
               05  FILLER              PIC X(1) VALUE X"5B".  *> [
               05  FILLER              PIC X(1) VALUE X"5C".  *> \
               05  FILLER              PIC X(1) VALUE X"5D".  *> ]
               05  FILLER              PIC X(1) VALUE X"5E".  *> ^
               05  FILLER              PIC X(1) VALUE X"5F".  *> _
               05  FILLER              PIC X(1) VALUE X"60".  *> `
               05  FILLER              PIC X(1) VALUE X"61".  *> A (LOW)
               05  FILLER              PIC X(1) VALUE X"62".  *> B (LOW)
               05  FILLER              PIC X(1) VALUE X"63".  *> C (LOW)
               05  FILLER              PIC X(1) VALUE X"64".  *> D (LOW)
               05  FILLER              PIC X(1) VALUE X"65".  *> E (LOW)
               05  FILLER              PIC X(1) VALUE X"66".  *> F (LOW)
               05  FILLER              PIC X(1) VALUE X"67".  *> G (LOW)
               05  FILLER              PIC X(1) VALUE X"68".  *> H (LOW)
               05  FILLER              PIC X(1) VALUE X"69".  *> I (LOW)
               05  FILLER              PIC X(1) VALUE X"6A".  *> J (LOW)
               05  FILLER              PIC X(1) VALUE X"6B".  *> K (LOW)
               05  FILLER              PIC X(1) VALUE X"6C".  *> L (LOW)
               05  FILLER              PIC X(1) VALUE X"6D".  *> M (LOW)
               05  FILLER              PIC X(1) VALUE X"6E".  *> N (LOW)
               05  FILLER              PIC X(1) VALUE X"6F".  *> O (LOW)
               05  FILLER              PIC X(1) VALUE X"70".  *> P (LOW)
               05  FILLER              PIC X(1) VALUE X"71".  *> Q (LOW)
               05  FILLER              PIC X(1) VALUE X"72".  *> R (LOW)
               05  FILLER              PIC X(1) VALUE X"73".  *> S (LOW)
               05  FILLER              PIC X(1) VALUE X"74".  *> T (LOW)
               05  FILLER              PIC X(1) VALUE X"75".  *> U (LOW)
               05  FILLER              PIC X(1) VALUE X"76".  *> V (LOW)
               05  FILLER              PIC X(1) VALUE X"77".  *> W (LOW)
               05  FILLER              PIC X(1) VALUE X"78".  *> X (LOW)
               05  FILLER              PIC X(1) VALUE X"79".  *> Y (LOW)
               05  FILLER              PIC X(1) VALUE X"7A".  *> Z (LOW)
               05  FILLER              PIC X(1) VALUE X"7B".  *> {
               05  FILLER              PIC X(1) VALUE X"7C".  *> |
               05  FILLER              PIC X(1) VALUE X"7D".  *> }
               05  FILLER              PIC X(1) VALUE X"7E".  *> ~
           03  SQL-TEXT-ASCIIX         REDEFINES SQL-TEXT-ASCII
                                       PIC X(01)
                                       OCCURS SQL-TEXT-ASCII-MAX.
           03  SQL-TEXT-AFTER          PIC X(01)     VALUE SPACES.
           03  SQL-TEXT-BEFORE         PIC X(01)     VALUE SPACES.
           03  SQL-TEXT-CURRENT        PIC X(01)     VALUE SPACES.

