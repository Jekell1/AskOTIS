      * COPYMEMBER: LIBIO/CONNECTW_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBIO/CONNECTW_SQL
      * DESCRIPTION   : SQL SERVER CONNECTION WORKERS
      * PRE-COMPILE   : IO/IOCONN.CBL
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
      *
      ******************************************************************
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-GP-KEY                      PIC 99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  SQL-CONNECT-WORKERS.
           03  SQL-CONNECT-SERVER          PIC X(30).
           03  SQL-CONNECT-USERID          PIC X(30).
           03  SQL-CONNECT-PSWD            PIC X(30).
      *??? 03  SQL-CONNECT-DATABASE        PIC X(20).
      *??? 03  SQL-CONNECT-OWNER           PIC X(20).
      *??? 03  SQL-CONNECT-PRE-FILE        PIC X(20).

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

