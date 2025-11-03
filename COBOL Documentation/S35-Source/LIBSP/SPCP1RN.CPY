      * COPYMEMBER: LIBSP/SPCP1RN
      ******************************************************************
      * NOTE: CP-PATH-OVERRIDE = "G" MEANS USE GLOBAL VERSION (GCPFILE)
      * BLM 210723 IF CP-PATH-OVERRIDE = "B" MOVE CP-PATH-OWNBR TO CP-BRNO
      *            ELSE MOVE 0
      ******************************************************************
       LOAD-CP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CP-ALL-DATA
              MOVE CP-ALL-PATH             TO CP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CP-ALL-BASE
              MOVE FILPATH-MACHINE         TO CP-ALL-MACHINE
              MOVE FILPATH-DATA            TO CP-ALL-DATA
              MOVE CP-ALL-PATH             TO CP-PATH.

      *-----------------------------------------------------------------
       OPEN-CP1-FILE.
           PERFORM LOAD-CP1-FILE.
           PERFORM OPEN-IT.
           MOVE CP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CP1-FILE.
           PERFORM START-IT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.

           IF ( CP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CP1-FILE.

           IF ( QCP1-WEND-ID = SPACES    ) 
              MOVE CP-BRNO              TO QCP1-WBEG-BRNO
              MOVE CP-BRNO              TO QCP1-WEND-BRNO
              MOVE CP-ID                TO QCP1-WBEG-ID
              MOVE ALL "Z"              TO QCP1-WEND-ID.

           EXEC SQL
            DECLARE CP1_CURSOR CURSOR FOR
            SELECT CPFILE.CP_BRNO,
                   CPFILE.CP_ID,
                   CAST(CPFILE.CP_LTOUCH_DATE AS VARCHAR(10)),
                   CPFILE.CP_DESC,
                   CPFILE.CP_COL_ID_1,
                   CPFILE.CP_COL_ID_2,
                   CPFILE.CP_COL_ID_3,
                   CPFILE.CP_COL_ID_4,
                   CPFILE.CP_COL_ID_5,
                   CPFILE.CP_COL_ID_6,
                   CPFILE.CP_COL_ID_7,
                   CPFILE.CP_COL_ID_8,
                   CPFILE.CP_COL_ID_9,
                   CPFILE.CP_COL_ID_10,
                   CPFILE.CP_COL_ID_11,
                   CPFILE.CP_COL_ID_12,
                   CPFILE.CP_COL_ID_13,
                   CPFILE.CP_COL_ID_14,
                   CPFILE.CP_COL_ID_15,
                   CPFILE.CP_COL_ID_16,
                   CPFILE.CP_COL_ID_17,
                   CPFILE.CP_COL_ID_18,
                   CPFILE.CP_COL_ID_19,
                   CPFILE.CP_COL_ID_20,
                   CPFILE.CP_COL_ID_21,
                   CPFILE.CP_COL_ID_22,
                   CPFILE.CP_COL_ID_23,
                   CPFILE.CP_COL_ID_24,
                   CPFILE.CP_COL_ID_25,
                   CPFILE.CP_COL_ID_26,
                   CPFILE.CP_COL_ID_27,
                   CPFILE.CP_COL_ID_28,
                   CPFILE.CP_COL_ID_29,
                   CPFILE.CP_COL_ID_30,
                   CPFILE.CP_COL_ID_31,
                   CPFILE.CP_COL_ID_32,
                   CPFILE.CP_COL_ID_33,
                   CPFILE.CP_COL_ID_34,
                   CPFILE.CP_COL_ID_35,
                   CPFILE.CP_COL_ID_36,
                   CPFILE.CP_COL_ID_37,
                   CPFILE.CP_COL_ID_38,
                   CPFILE.CP_COL_ID_39,
                   CPFILE.CP_COL_ID_40,
                   CPFILE.CP_COL_ID_41,
                   CPFILE.CP_COL_ID_42,
                   CPFILE.CP_COL_ID_43,
                   CPFILE.CP_COL_ID_44,
                   CPFILE.CP_COL_ID_45,
                   CPFILE.CP_COL_ID_46,
                   CPFILE.CP_COL_ID_47,
                   CPFILE.CP_COL_ID_48,
                   CPFILE.CP_COL_ID_49,
                   CPFILE.CP_COL_ID_50,
                   CPFILE.CP_COL_ID_51,
                   CPFILE.CP_COL_ID_52,
                   CPFILE.CP_COL_ID_53,
                   CPFILE.CP_COL_ID_54,
                   CPFILE.CP_COL_ID_55,
                   CPFILE.CP_COL_ID_56,
                   CPFILE.CP_COL_ID_57,
                   CPFILE.CP_COL_ID_58,
                   CPFILE.CP_COL_ID_59,
                   CPFILE.CP_COL_ID_60,
                   CPFILE.CP_COL_ID_61,
                   CPFILE.CP_COL_ID_62,
                   CPFILE.CP_COL_ID_63,
                   CPFILE.CP_COL_ID_64,
                   CPFILE.CP_COL_ID_65,
                   CPFILE.CP_COL_ID_66,
                   CPFILE.CP_COL_ID_67,
                   CPFILE.CP_COL_ID_68,
                   CPFILE.CP_COL_ID_69,
                   CPFILE.CP_COL_ID_70,
                   CPFILE.CP_COL_ID_71,
                   CPFILE.CP_COL_ID_72,
                   CPFILE.CP_COL_ID_73,
                   CPFILE.CP_COL_ID_74,
                   CPFILE.CP_COL_ID_75,
                   CPFILE.CP_COL_ID_76,
                   CPFILE.CP_COL_ID_77,
                   CPFILE.CP_COL_ID_78,
                   CPFILE.CP_COL_ID_79,
                   CPFILE.CP_COL_ID_80,
                   CPFILE.CP_ALPHA_FLAG,
                   CPFILE.CP_TOTAL_ACCOUNTS,
                   CPFILE.CP_COLLECTOR_COUNT,
                   CPFILE.CP_LAST_SUBSCRIPT
            FROM DBO.CPFILE
            WHERE CPFILE.CP_BRNO = :QCP1-WBEG-BRNO
              AND CPFILE.CP_ID >= :QCP1-WBEG-ID
              AND CPFILE.CP_ID <= :QCP1-WEND-ID
            ORDER BY CPFILE.CP_BRNO,
                     CPFILE.CP_ID
           END-EXEC.

           EXEC SQL
               OPEN CP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCP1-WBEG-BRNO 
                          QCP1-WEND-BRNO.
 
           MOVE SPACES TO QCP1-WBEG-ID
                          QCP1-WEND-ID.

           MOVE STAT---GOOD TO CP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CP1-FILE.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           PERFORM READ-IT.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           INITIALIZE QCP-REC.

           MOVE CP-BRNO   TO QCP-BRNO.
           MOVE CP-ID     TO QCP-ID.

           EXEC SQL
            SELECT CPFILE.CP_BRNO,
                   CPFILE.CP_ID,
                   CAST(CPFILE.CP_LTOUCH_DATE AS VARCHAR(10)),
                   CPFILE.CP_DESC,
                   CPFILE.CP_COL_ID_1,
                   CPFILE.CP_COL_ID_2,
                   CPFILE.CP_COL_ID_3,
                   CPFILE.CP_COL_ID_4,
                   CPFILE.CP_COL_ID_5,
                   CPFILE.CP_COL_ID_6,
                   CPFILE.CP_COL_ID_7,
                   CPFILE.CP_COL_ID_8,
                   CPFILE.CP_COL_ID_9,
                   CPFILE.CP_COL_ID_10,
                   CPFILE.CP_COL_ID_11,
                   CPFILE.CP_COL_ID_12,
                   CPFILE.CP_COL_ID_13,
                   CPFILE.CP_COL_ID_14,
                   CPFILE.CP_COL_ID_15,
                   CPFILE.CP_COL_ID_16,
                   CPFILE.CP_COL_ID_17,
                   CPFILE.CP_COL_ID_18,
                   CPFILE.CP_COL_ID_19,
                   CPFILE.CP_COL_ID_20,
                   CPFILE.CP_COL_ID_21,
                   CPFILE.CP_COL_ID_22,
                   CPFILE.CP_COL_ID_23,
                   CPFILE.CP_COL_ID_24,
                   CPFILE.CP_COL_ID_25,
                   CPFILE.CP_COL_ID_26,
                   CPFILE.CP_COL_ID_27,
                   CPFILE.CP_COL_ID_28,
                   CPFILE.CP_COL_ID_29,
                   CPFILE.CP_COL_ID_30,
                   CPFILE.CP_COL_ID_31,
                   CPFILE.CP_COL_ID_32,
                   CPFILE.CP_COL_ID_33,
                   CPFILE.CP_COL_ID_34,
                   CPFILE.CP_COL_ID_35,
                   CPFILE.CP_COL_ID_36,
                   CPFILE.CP_COL_ID_37,
                   CPFILE.CP_COL_ID_38,
                   CPFILE.CP_COL_ID_39,
                   CPFILE.CP_COL_ID_40,
                   CPFILE.CP_COL_ID_41,
                   CPFILE.CP_COL_ID_42,
                   CPFILE.CP_COL_ID_43,
                   CPFILE.CP_COL_ID_44,
                   CPFILE.CP_COL_ID_45,
                   CPFILE.CP_COL_ID_46,
                   CPFILE.CP_COL_ID_47,
                   CPFILE.CP_COL_ID_48,
                   CPFILE.CP_COL_ID_49,
                   CPFILE.CP_COL_ID_50,
                   CPFILE.CP_COL_ID_51,
                   CPFILE.CP_COL_ID_52,
                   CPFILE.CP_COL_ID_53,
                   CPFILE.CP_COL_ID_54,
                   CPFILE.CP_COL_ID_55,
                   CPFILE.CP_COL_ID_56,
                   CPFILE.CP_COL_ID_57,
                   CPFILE.CP_COL_ID_58,
                   CPFILE.CP_COL_ID_59,
                   CPFILE.CP_COL_ID_60,
                   CPFILE.CP_COL_ID_61,
                   CPFILE.CP_COL_ID_62,
                   CPFILE.CP_COL_ID_63,
                   CPFILE.CP_COL_ID_64,
                   CPFILE.CP_COL_ID_65,
                   CPFILE.CP_COL_ID_66,
                   CPFILE.CP_COL_ID_67,
                   CPFILE.CP_COL_ID_68,
                   CPFILE.CP_COL_ID_69,
                   CPFILE.CP_COL_ID_70,
                   CPFILE.CP_COL_ID_71,
                   CPFILE.CP_COL_ID_72,
                   CPFILE.CP_COL_ID_73,
                   CPFILE.CP_COL_ID_74,
                   CPFILE.CP_COL_ID_75,
                   CPFILE.CP_COL_ID_76,
                   CPFILE.CP_COL_ID_77,
                   CPFILE.CP_COL_ID_78,
                   CPFILE.CP_COL_ID_79,
                   CPFILE.CP_COL_ID_80,
                   CPFILE.CP_ALPHA_FLAG,
                   CPFILE.CP_TOTAL_ACCOUNTS,
                   CPFILE.CP_COLLECTOR_COUNT,
                   CPFILE.CP_LAST_SUBSCRIPT
            INTO :QCP-REC
            FROM DBO.CPFILE
            WHERE CPFILE.CP_BRNO = :QCP-BRNO 
              AND CPFILE.CP_ID = :QCP-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CP-FIELDS.

      *-----------------------------------------------------------------
       READ-CP1-FILE-NEXT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           PERFORM READ-IT.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           INITIALIZE QCP-REC.

           IF ( CP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CP1_CURSOR INTO :QCP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CP1-FILE.

           PERFORM CLOSE-IT.
           IF ( CP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CP1-CURSOR-STAT.

