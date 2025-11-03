      * COPYMEMBER: LIBSP/SPCP1IN
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

           IF ( QCP1-WEND-ID = SPACES )
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
           PERFORM READ-IT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
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
           PERFORM READ-IT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0 TO CP-BRNO.
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
       WRITE-CP1-FILE.

           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           ACCEPT CP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           PERFORM SET-CP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CPFILE
             (CPFILE.CP_BRNO,
              CPFILE.CP_ID,
              CPFILE.CP_LTOUCH_DATE,
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
              CPFILE.CP_LAST_SUBSCRIPT)
            VALUES
             (:QCP-BRNO,
              :QCP-ID,
              :QCP-LTOUCH-DATE,
              :QCP-DESC,
              :QCP-COL-ID-1,
              :QCP-COL-ID-2,
              :QCP-COL-ID-3,
              :QCP-COL-ID-4,
              :QCP-COL-ID-5,
              :QCP-COL-ID-6,
              :QCP-COL-ID-7,
              :QCP-COL-ID-8,
              :QCP-COL-ID-9,
              :QCP-COL-ID-10,
              :QCP-COL-ID-11,
              :QCP-COL-ID-12,
              :QCP-COL-ID-13,
              :QCP-COL-ID-14,
              :QCP-COL-ID-15,
              :QCP-COL-ID-16,
              :QCP-COL-ID-17,
              :QCP-COL-ID-18,
              :QCP-COL-ID-19,
              :QCP-COL-ID-20,
              :QCP-COL-ID-21,
              :QCP-COL-ID-22,
              :QCP-COL-ID-23,
              :QCP-COL-ID-24,
              :QCP-COL-ID-25,
              :QCP-COL-ID-26,
              :QCP-COL-ID-27,
              :QCP-COL-ID-28,
              :QCP-COL-ID-29,
              :QCP-COL-ID-30,
              :QCP-COL-ID-31,
              :QCP-COL-ID-32,
              :QCP-COL-ID-33,
              :QCP-COL-ID-34,
              :QCP-COL-ID-35,
              :QCP-COL-ID-36,
              :QCP-COL-ID-37,
              :QCP-COL-ID-38,
              :QCP-COL-ID-39,
              :QCP-COL-ID-40,
              :QCP-COL-ID-41,
              :QCP-COL-ID-42,
              :QCP-COL-ID-43,
              :QCP-COL-ID-44,
              :QCP-COL-ID-45,
              :QCP-COL-ID-46,
              :QCP-COL-ID-47,
              :QCP-COL-ID-48,
              :QCP-COL-ID-49,
              :QCP-COL-ID-50,
              :QCP-COL-ID-51,
              :QCP-COL-ID-52,
              :QCP-COL-ID-53,
              :QCP-COL-ID-54,
              :QCP-COL-ID-55,
              :QCP-COL-ID-56,
              :QCP-COL-ID-57,
              :QCP-COL-ID-58,
              :QCP-COL-ID-59,
              :QCP-COL-ID-60,
              :QCP-COL-ID-61,
              :QCP-COL-ID-62,
              :QCP-COL-ID-63,
              :QCP-COL-ID-64,
              :QCP-COL-ID-65,
              :QCP-COL-ID-66,
              :QCP-COL-ID-67,
              :QCP-COL-ID-68,
              :QCP-COL-ID-69,
              :QCP-COL-ID-70,
              :QCP-COL-ID-71,
              :QCP-COL-ID-72,
              :QCP-COL-ID-73,
              :QCP-COL-ID-74,
              :QCP-COL-ID-75,
              :QCP-COL-ID-76,
              :QCP-COL-ID-77,
              :QCP-COL-ID-78,
              :QCP-COL-ID-79,
              :QCP-COL-ID-80,
              :QCP-ALPHA-FLAG,
              :QCP-TOTAL-ACCOUNTS,
              :QCP-COLLECTOR-COUNT,
              :QCP-LAST-SUBSCRIPT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CP1-FILE.

           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           ACCEPT CP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           PERFORM SET-CP-FIELDS.

           EXEC SQL
            UPDATE DBO.CPFILE
             SET
              CPFILE.CP_BRNO = :QCP-BRNO,
              CPFILE.CP_ID = :QCP-ID,
              CPFILE.CP_LTOUCH_DATE = :QCP-LTOUCH-DATE,
              CPFILE.CP_DESC = :QCP-DESC,
              CPFILE.CP_COL_ID_1 = :QCP-COL-ID-1,
              CPFILE.CP_COL_ID_2 = :QCP-COL-ID-2,
              CPFILE.CP_COL_ID_3 = :QCP-COL-ID-3,
              CPFILE.CP_COL_ID_4 = :QCP-COL-ID-4,
              CPFILE.CP_COL_ID_5 = :QCP-COL-ID-5,
              CPFILE.CP_COL_ID_6 = :QCP-COL-ID-6,
              CPFILE.CP_COL_ID_7 = :QCP-COL-ID-7,
              CPFILE.CP_COL_ID_8 = :QCP-COL-ID-8,
              CPFILE.CP_COL_ID_9 = :QCP-COL-ID-9,
              CPFILE.CP_COL_ID_10 = :QCP-COL-ID-10,
              CPFILE.CP_COL_ID_11 = :QCP-COL-ID-11,
              CPFILE.CP_COL_ID_12 = :QCP-COL-ID-12,
              CPFILE.CP_COL_ID_13 = :QCP-COL-ID-13,
              CPFILE.CP_COL_ID_14 = :QCP-COL-ID-14,
              CPFILE.CP_COL_ID_15 = :QCP-COL-ID-15,
              CPFILE.CP_COL_ID_16 = :QCP-COL-ID-16,
              CPFILE.CP_COL_ID_17 = :QCP-COL-ID-17,
              CPFILE.CP_COL_ID_18 = :QCP-COL-ID-18,
              CPFILE.CP_COL_ID_19 = :QCP-COL-ID-19,
              CPFILE.CP_COL_ID_20 = :QCP-COL-ID-20,
              CPFILE.CP_COL_ID_21 = :QCP-COL-ID-21,
              CPFILE.CP_COL_ID_22 = :QCP-COL-ID-22,
              CPFILE.CP_COL_ID_23 = :QCP-COL-ID-23,
              CPFILE.CP_COL_ID_24 = :QCP-COL-ID-24,
              CPFILE.CP_COL_ID_25 = :QCP-COL-ID-25,
              CPFILE.CP_COL_ID_26 = :QCP-COL-ID-26,
              CPFILE.CP_COL_ID_27 = :QCP-COL-ID-27,
              CPFILE.CP_COL_ID_28 = :QCP-COL-ID-28,
              CPFILE.CP_COL_ID_29 = :QCP-COL-ID-29,
              CPFILE.CP_COL_ID_30 = :QCP-COL-ID-30,
              CPFILE.CP_COL_ID_31 = :QCP-COL-ID-31,
              CPFILE.CP_COL_ID_32 = :QCP-COL-ID-32,
              CPFILE.CP_COL_ID_33 = :QCP-COL-ID-33,
              CPFILE.CP_COL_ID_34 = :QCP-COL-ID-34,
              CPFILE.CP_COL_ID_35 = :QCP-COL-ID-35,
              CPFILE.CP_COL_ID_36 = :QCP-COL-ID-36,
              CPFILE.CP_COL_ID_37 = :QCP-COL-ID-37,
              CPFILE.CP_COL_ID_38 = :QCP-COL-ID-38,
              CPFILE.CP_COL_ID_39 = :QCP-COL-ID-39,
              CPFILE.CP_COL_ID_40 = :QCP-COL-ID-40,
              CPFILE.CP_COL_ID_41 = :QCP-COL-ID-41,
              CPFILE.CP_COL_ID_42 = :QCP-COL-ID-42,
              CPFILE.CP_COL_ID_43 = :QCP-COL-ID-43,
              CPFILE.CP_COL_ID_44 = :QCP-COL-ID-44,
              CPFILE.CP_COL_ID_45 = :QCP-COL-ID-45,
              CPFILE.CP_COL_ID_46 = :QCP-COL-ID-46,
              CPFILE.CP_COL_ID_47 = :QCP-COL-ID-47,
              CPFILE.CP_COL_ID_48 = :QCP-COL-ID-48,
              CPFILE.CP_COL_ID_49 = :QCP-COL-ID-49,
              CPFILE.CP_COL_ID_50 = :QCP-COL-ID-50,
              CPFILE.CP_COL_ID_51 = :QCP-COL-ID-51,
              CPFILE.CP_COL_ID_52 = :QCP-COL-ID-52,
              CPFILE.CP_COL_ID_53 = :QCP-COL-ID-53,
              CPFILE.CP_COL_ID_54 = :QCP-COL-ID-54,
              CPFILE.CP_COL_ID_55 = :QCP-COL-ID-55,
              CPFILE.CP_COL_ID_56 = :QCP-COL-ID-56,
              CPFILE.CP_COL_ID_57 = :QCP-COL-ID-57,
              CPFILE.CP_COL_ID_58 = :QCP-COL-ID-58,
              CPFILE.CP_COL_ID_59 = :QCP-COL-ID-59,
              CPFILE.CP_COL_ID_60 = :QCP-COL-ID-60,
              CPFILE.CP_COL_ID_61 = :QCP-COL-ID-61,
              CPFILE.CP_COL_ID_62 = :QCP-COL-ID-62,
              CPFILE.CP_COL_ID_63 = :QCP-COL-ID-63,
              CPFILE.CP_COL_ID_64 = :QCP-COL-ID-64,
              CPFILE.CP_COL_ID_65 = :QCP-COL-ID-65,
              CPFILE.CP_COL_ID_66 = :QCP-COL-ID-66,
              CPFILE.CP_COL_ID_67 = :QCP-COL-ID-67,
              CPFILE.CP_COL_ID_68 = :QCP-COL-ID-68,
              CPFILE.CP_COL_ID_69 = :QCP-COL-ID-69,
              CPFILE.CP_COL_ID_70 = :QCP-COL-ID-70,
              CPFILE.CP_COL_ID_71 = :QCP-COL-ID-71,
              CPFILE.CP_COL_ID_72 = :QCP-COL-ID-72,
              CPFILE.CP_COL_ID_73 = :QCP-COL-ID-73,
              CPFILE.CP_COL_ID_74 = :QCP-COL-ID-74,
              CPFILE.CP_COL_ID_75 = :QCP-COL-ID-75,
              CPFILE.CP_COL_ID_76 = :QCP-COL-ID-76,
              CPFILE.CP_COL_ID_77 = :QCP-COL-ID-77,
              CPFILE.CP_COL_ID_78 = :QCP-COL-ID-78,
              CPFILE.CP_COL_ID_79 = :QCP-COL-ID-79,
              CPFILE.CP_COL_ID_80 = :QCP-COL-ID-80,
              CPFILE.CP_ALPHA_FLAG = :QCP-ALPHA-FLAG,
              CPFILE.CP_TOTAL_ACCOUNTS = :QCP-TOTAL-ACCOUNTS,
              CPFILE.CP_COLLECTOR_COUNT = :QCP-COLLECTOR-COUNT,
              CPFILE.CP_LAST_SUBSCRIPT = :QCP-LAST-SUBSCRIPT
             WHERE CPFILE.CP_BRNO = :QCP-BRNO 
               AND CPFILE.CP_ID = :QCP-ID
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CP1-FILE.

           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           PERFORM DELETE-IT.
           MOVE CP-PATH-SQL    TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           PERFORM SET-CP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CPFILE
             WHERE CPFILE.CP_BRNO = :QCP-BRNO 
               AND CPFILE.CP_ID = :QCP-ID
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CP1-FILE.

           PERFORM CLOSE-IT.

           IF ( CP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CP1-CURSOR-STAT.

