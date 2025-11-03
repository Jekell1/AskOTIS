      * COPYMEMBER: LIBUP/UG1534_IO.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UG1534_IO.CPY
      *
      * DESCRIPTION   : IO FOR SOURCE (A15) & TARGET (A34) GLOBAL FILES
      *
      *=================================================================

      * GLOBALS:

       COPY "LIBGB/GBPEGS_SQL.CPY".
       COPY "LIBGB/GBPE1IN.CPY".
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPW1IN.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBSEGS_SQL.CPY".
       COPY "LIBGB/GBSE1IN.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBSN1IN.CPY".

       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGL/GLAL1IN.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGL/GLGI1IN.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGT1IN.CPY".

       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1IN.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCL1IN.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPEA1IN.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPLC1IN.CPY".
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBLP/LPSM1IN.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPSSGS_SQL.CPY".
       COPY "LIBLP/LPSS1IN.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPST1IN.CPY".
       COPY "LIBSP/SPTRWINFOGS_SQL.CPY".
       COPY "LIBSP/SPTRWINFOIN.CPY".
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPTZ1IN.CPY".

       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBSP/SPCP1IN.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBSP/SPWR1IN.CPY".
       COPY "LIBGB/GBAUGS_SQL.CPY".
       COPY "LIBGB/GBAU1IN.CPY".
       COPY "LIBSP/SPIIGS_SQL.CPY".
       COPY "LIBSP/SPIIIN.CPY".
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPIRIN.CPY".

      * LOCALS:

       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPCI1IN.CPY".

       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBSP/SPCJ1IN.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPIC1IN.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPIX1IN.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
      * IT DOESNT CREATE BI 
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN_NOBI.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLSD1IN.CPY".
       COPY "LIBLP/LPLSGGS_SQL.CPY".
       COPY "LIBLP/LPLSG1IN.CPY".
       COPY "LIBLP/LPLSFGS_SQL.CPY".
       COPY "LIBLP/LPLSF1IN.CPY".
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSJ1IN.CPY".
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLSL1IN.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLSPGS_SQL.CPY".
       COPY "LIBLP/LPLSP1IN.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSX1IN.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTC1IN_NOIX.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTD1IN.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLTF1IN.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
      *COPY "LIBLP/LPMLGS_SQL.CPY".
      *COPY "LIBLP/LPML1IN.CPY".
      *COPY "LIBLP/LPMPGS_SQL.CPY".
      *COPY "LIBLP/LPMP1IN.CPY".
      *COPY "LIBLP/LPPAGS_SQL.CPY".
      *COPY "LIBLP/LPPA1IN.CPY".
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPRX1IN.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTY1IN.CPY".


      ******************************************************************
      *
      *    S O U R C E - F I L E S - A 1 5
      *
      ******************************************************************

      *=================================================================
      *    LIBGB/GBPE1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBPE1RN
       OPEN-OPE1-FILE.
           PERFORM OPEN-IT.
           MOVE OPE-PATH TO E-FILE.
           OPEN INPUT OPE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPE1-FILE.
           IF IO-FG = 7
              CLOSE OPE-FILE
              GO TO OPEN-OPE1-FILE.
           UNLOCK OPE-FILE.
       READ-OPE1-FILE.
           PERFORM READ-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           READ OPE-FILE.
           IF IO-FG = 8
              GO TO READ-OPE1-FILE.
           UNLOCK OPE-FILE.
       READ-OPE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           READ OPE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPE1-FILE-NEXT.
           UNLOCK OPE-FILE.
       START-OPE1-FILE.
           PERFORM START-IT.
           MOVE OPE-PATH TO E-FILE.
           MOVE OPE1-KEY TO E-KEYX.
           START OPE-FILE KEY NOT < OPE1-KEY.
           UNLOCK OPE-FILE.
       CLOSE-OPE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPE-FILE.

      *=================================================================
      *    LIBGB/GBPW1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBPW1RN
       OPEN-OPW1-FILE.
           PERFORM OPEN-IT.
           MOVE OPW-PATH TO E-FILE.
           OPEN INPUT OPW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPW1-FILE.
           IF IO-FG = 7
              CLOSE OPW-FILE
              GO TO OPEN-OPW1-FILE.
           UNLOCK OPW-FILE.
       READ-OPW1-FILE.
           PERFORM READ-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           READ OPW-FILE.
           IF IO-FG = 8
              GO TO READ-OPW1-FILE.
           UNLOCK OPW-FILE.
       READ-OPW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           READ OPW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OPW1-FILE-NEXT.
           UNLOCK OPW-FILE.
       START-OPW1-FILE.
           PERFORM START-IT.
           MOVE OPW-PATH TO E-FILE.
           MOVE OPW1-KEY TO E-KEYX.
           START OPW-FILE KEY NOT < OPW1-KEY.
           UNLOCK OPW-FILE.
       CLOSE-OPW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OPW-FILE.

      *=================================================================
      *    LIBGB/GBRC1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBRC1RN
       OPEN-ORC1-FILE.
           PERFORM OPEN-IT.
           MOVE ORC-PATH TO E-FILE.
           OPEN INPUT ORC-FILE.
           IF IO-FG = 8
              GO TO OPEN-ORC1-FILE.
           IF IO-FG = 7
              CLOSE ORC-FILE
              GO TO OPEN-ORC1-FILE.
       READ-ORC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE-NEXT.
       READ-ORC1-FILE.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE.
       START-ORC1-FILE.
           PERFORM START-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           START ORC-FILE KEY NOT < ORC1-KEY.
       CLOSE-ORC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ORC-FILE.

      *=================================================================
      *    LIBGB/GBSE1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBSE1RN
       OPEN-OSE1-FILE.
           PERFORM OPEN-IT.
           MOVE OSE-PATH TO E-FILE.
           OPEN INPUT OSE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSE1-FILE.
           IF IO-FG = 7
              CLOSE OSE-FILE
              GO TO OPEN-OSE1-FILE.
       READ-OSE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           READ OSE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSE1-FILE-NEXT.
       START-OSE1-FILE.
           PERFORM START-IT.
           MOVE OSE-PATH TO E-FILE.
           MOVE OSE1-KEY TO E-KEYX.
           START OSE-FILE KEY NOT < OSE1-KEY.
       CLOSE-OSE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSE-FILE.

      *=================================================================
      *    LIBGB/GBSN1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBSN1RN
       OPEN-OSN1-FILE.
           PERFORM OPEN-IT.
           MOVE OSN-PATH TO E-FILE.
           OPEN INPUT OSN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSN1-FILE.
           IF IO-FG = 7
              CLOSE OSN-FILE
              GO TO OPEN-OSN1-FILE.
       READ-OSN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           READ OSN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSN1-FILE-NEXT.
       START-OSN1-FILE.
           PERFORM START-IT.
           MOVE OSN-PATH TO E-FILE.
           MOVE OSN1-KEY TO E-KEYX.
           START OSN-FILE KEY NOT < OSN1-KEY.
       CLOSE-OSN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSN-FILE.

      *=================================================================
      *    LIBGL/GLAL1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGL/GLAL1RN
       OPEN-OAL1-FILE.
           PERFORM OPEN-IT.
           MOVE OAL-PATH TO E-FILE.
           OPEN INPUT OAL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAL1-FILE.
           IF IO-FG = 7
              CLOSE OAL-FILE
              GO TO OPEN-OAL1-FILE.
       READ-OAL1-FILE.
           PERFORM READ-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           READ OAL-FILE.
           IF IO-FG = 8
              GO TO READ-OAL1-FILE.
       READ-OAL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           READ OAL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAL1-FILE-NEXT.
       START-OAL1-FILE.
           PERFORM START-IT.
           MOVE OAL-PATH TO E-FILE.
           MOVE OAL1-KEY TO E-KEYX.
           START OAL-FILE KEY NOT < OAL1-KEY.
       CLOSE-OAL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAL-FILE.

      *=================================================================
      *    LIBGL/GLGI1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGL/GLGI1RN
       OPEN-OGI1-FILE.
           PERFORM OPEN-IT.
           MOVE OGI-PATH TO E-FILE.
           OPEN INPUT OGI-FILE.
           IF IO-FG = 8
             GO TO OPEN-OGI1-FILE.
           IF IO-FG = 7
             CLOSE OGI-FILE
             GO TO OPEN-OGI1-FILE.
           UNLOCK OGI-FILE.
       READ-OGI1-FILE.
           PERFORM READ-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           READ OGI-FILE.
           IF IO-FG = 8
              GO TO READ-OGI1-FILE.
           UNLOCK OGI-FILE.
       READ-OGI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           READ OGI-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OGI1-FILE-NEXT.
           UNLOCK OGI-FILE.
       START-OGI1-FILE.
           PERFORM START-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           START OGI-FILE KEY NOT < OGI1-KEY.
           UNLOCK OGI-FILE.
       CLOSE-OGI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGI-FILE.

      *=================================================================
      *    LIBGL/GLGL1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGL/GLGL1RN
       OPEN-OGL1-FILE.
           PERFORM OPEN-IT.
           MOVE OGL-PATH TO E-FILE.
           OPEN INPUT OGL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGL1-FILE.
           IF IO-FG = 7
              CLOSE OGL-FILE
              GO TO OPEN-OGL1-FILE.
       READ-OGL1-FILE.
           PERFORM READ-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           READ OGL-FILE.
           IF IO-FG = 8
              GO TO READ-OGL1-FILE.
       READ-OGL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           READ OGL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGL1-FILE-NEXT.
       START-OGL1-FILE.
           PERFORM START-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           START OGL-FILE KEY NOT < OGL1-KEY.
       START-OGL1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           START OGL-FILE KEY > OGL1-KEY.
       CLOSE-OGL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGL-FILE.

      *=================================================================
      *    LIBGL/GLGT1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBGL/GLGT1RN
       OPEN-OGT1-FILE.
           PERFORM OPEN-IT.
           MOVE OGT-PATH TO E-FILE.
           OPEN INPUT OGT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGT1-FILE.
           IF IO-FG = 7
              CLOSE OGT-FILE
              GO TO OPEN-OGT1-FILE.
       READ-OGT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT1-KEY TO E-KEYX.
           READ OGT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGT1-FILE-NEXT.
       START-OGT1-FILE.
           PERFORM START-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT1-KEY TO E-KEYX.
           START OGT-FILE KEY NOT < OGT1-KEY.
       CLOSE-OGT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGT-FILE.


      *=================================================================
      *    LIBLP/LPBY1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPBY1RN
       OPEN-OBY1-FILE.
           PERFORM OPEN-IT.
           MOVE OBY-PATH TO E-FILE.
           OPEN INPUT OBY-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBY1-FILE.
           IF IO-FG = 7
              CLOSE OBY-FILE
              GO TO OPEN-OBY1-FILE.
       READ-OBY1-FILE.
           PERFORM READ-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           READ OBY-FILE.
           IF IO-FG = 8
              GO TO READ-OBY1-FILE.
       READ-OBY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           READ OBY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBY1-FILE-NEXT.
       START-OBY1-FILE.
           PERFORM START-IT.
           MOVE OBY-PATH TO E-FILE.
           MOVE OBY1-KEY TO E-KEYX.
           START OBY-FILE KEY NOT < OBY1-KEY.
       CLOSE-OBY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBY-FILE.

      *=================================================================
      *    LIBLP/LPCD1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPCD1RN
       OPEN-OCD1-FILE.
           PERFORM OPEN-IT.
           MOVE OCD-PATH TO E-FILE.
           OPEN INPUT OCD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCD1-FILE.
           IF IO-FG = 7
              CLOSE OCD-FILE
              GO TO OPEN-OCD1-FILE.
       READ-OCD1-FILE.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           READ OCD-FILE.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE.
       READ-OCD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           READ OCD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE-NEXT.
       START-OCD1-FILE.
           PERFORM START-IT.
           MOVE OCD-PATH TO E-FILE.
           MOVE OCD1-KEY TO E-KEYX.
           START OCD-FILE KEY NOT < OCD1-KEY.
       CLOSE-OCD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCD-FILE.

      *=================================================================
      *    LIBLP/LPCL1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPCL1RN
       OPEN-OCL1-FILE.
           PERFORM OPEN-IT.
           MOVE OCL-PATH TO E-FILE.
           OPEN INPUT OCL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCL1-FILE.
           IF IO-FG = 7
              CLOSE OCL-FILE
              GO TO OPEN-OCL1-FILE.
       READ-OCL1-FILE.
           PERFORM READ-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           READ OCL-FILE.
       READ-OCL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           READ OCL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCL1-FILE-NEXT.
       START-OCL1-FILE.
           PERFORM START-IT.
           MOVE OCL-PATH TO E-FILE.
           MOVE OCL1-KEY TO E-KEYX.
           START OCL-FILE KEY NOT < OCL1-KEY.
       CLOSE-OCL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCL-FILE.

      *=================================================================
      *    LIBLP/LPEA1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPEA1RN
       OPEN-OEA1-FILE.
           PERFORM OPEN-IT.
           MOVE OEA-PATH TO E-FILE.
           OPEN INPUT OEA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEA1-FILE.
           IF IO-FG = 7
              CLOSE OEA-FILE
              GO TO OPEN-OEA1-FILE.
       READ-OEA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           READ OEA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEA1-FILE-NEXT.
       START-OEA1-FILE.
           PERFORM START-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           START OEA-FILE KEY NOT < OEA1-KEY.
       CLOSE-OEA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEA-FILE.

      *=================================================================
      *    LIBLP/LPFD1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPFD1RN
      * OPEN-OFD1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OFD-PATH TO E-FILE.
      *     OPEN INPUT OFD-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-OFD1-FILE.
      *     IF IO-FG = 7
      *        CLOSE OFD-FILE
      *        GO TO OPEN-OFD1-FILE.
      * READ-OFD1-FILE.
      *     PERFORM READ-IT.
      *     MOVE OFD-PATH TO E-FILE.
      *     MOVE OFD1-KEY TO E-KEYX.
      *     READ OFD-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-OFD1-FILE.
      * READ-OFD1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OFD-PATH TO E-FILE.
      *     MOVE OFD1-KEY TO E-KEYX.
      *     READ OFD-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-OFD1-FILE-NEXT.
      * START-OFD1-FILE.
      *     PERFORM START-IT.
      *     MOVE OFD-PATH TO E-FILE.
      *     MOVE OFD1-KEY TO E-KEYX.
      *     START OFD-FILE KEY NOT < OFD1-KEY.
      * CLOSE-OFD1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OFD-FILE.

      *=================================================================
      *    LIBLP/LPFT1RN --- LAST UPDATED: 2016-0118 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPFT1RN
      * OPEN-OFT1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OFT-PATH TO E-FILE.
      *     OPEN INPUT OFT-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-OFT1-FILE.
      *     IF IO-FG = 7
      *        CLOSE OFT-FILE
      *        GO TO OPEN-OFT1-FILE.
      * READ-OFT1-FILE.
      *     PERFORM READ-IT.
      *     MOVE OFT-PATH TO E-FILE.
      *     MOVE OFT1-KEY TO E-KEYX.
      *     READ OFT-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-OFT1-FILE.
      * READ-OFT1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OFT-PATH TO E-FILE.
      *     MOVE OFT1-KEY TO E-KEYX.
      *     READ OFT-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-OFT1-FILE-NEXT.
      * START-OFT1-FILE.
      *     PERFORM START-IT.
      *     MOVE OFT-PATH TO E-FILE.
      *     MOVE OFT1-KEY TO E-KEYX.
      *     START OFT-FILE KEY NOT < OFT1-KEY.
      * CLOSE-OFT1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OFT-FILE.

      *=================================================================
      *    LIBLP/LPLC1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPLC1RN
       OPEN-OLC1-FILE.
           PERFORM OPEN-IT.
           MOVE OLC-PATH TO E-FILE.
           OPEN INPUT OLC-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLC1-FILE.
           IF IO-FG = 7
              CLOSE OLC-FILE
              GO TO OPEN-OLC1-FILE.
       READ-OLC1-FILE.
           PERFORM READ-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           READ OLC-FILE.
           IF IO-FG = 8
              GO TO READ-OLC1-FILE.
       READ-OLC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           READ OLC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLC1-FILE-NEXT.
       START-OLC1-FILE.
           PERFORM START-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           START OLC-FILE KEY NOT < OLC1-KEY.
       START-OLC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OLC-PATH TO E-FILE.
           MOVE OLC1-KEY TO E-KEYX.
           START OLC-FILE KEY > OLC1-KEY.
           UNLOCK OLC-FILE.
       CLOSE-OLC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLC-FILE.

      *=================================================================
      *    LIBLP/LPSM1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPSM1RN
       OPEN-OSM1-FILE.
           PERFORM OPEN-IT.
           MOVE OSM-PATH TO E-FILE.
           OPEN I-O OSM-FILE.
           IF IO-FG = 8
             GO TO OPEN-OSM1-FILE.
           IF IO-FG = 7
             CLOSE OSM-FILE
             GO TO OPEN-OSM1-FILE.
           UNLOCK OSM-FILE.
       READ-OSM1-FILE.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           READ OSM-FILE.
           IF IO-FG = 8
              GO TO READ-OSM1-FILE.
       READ-OSM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           READ OSM-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OSM1-FILE-NEXT.
       START-OSM1-FILE.
           PERFORM START-IT.
           MOVE OSM-PATH TO E-FILE.
           MOVE OSM1-KEY TO E-KEYX.
           START OSM-FILE KEY NOT < OSM1-KEY.
           UNLOCK OSM-FILE.
       CLOSE-OSM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSM-FILE.

      *=================================================================
      *    LIBLP/LPSP1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPSP1RN
       OPEN-OSP1-FILE.
           PERFORM OPEN-IT.
           MOVE OSP-PATH TO E-FILE.
           OPEN INPUT OSP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSP1-FILE.
           IF IO-FG = 7
              CLOSE OSP-FILE
              GO TO OPEN-OSP1-FILE.
       READ-OSP1-FILE.
           PERFORM READ-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           READ OSP-FILE.
           IF IO-FG = 8
              GO TO READ-OSP1-FILE.
       READ-OSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           READ OSP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSP1-FILE-NEXT.
       START-OSP1-FILE.
           PERFORM START-IT.
           MOVE OSP-PATH TO E-FILE.
           MOVE OSP1-KEY TO E-KEYX.
           START OSP-FILE KEY NOT < OSP1-KEY.
       CLOSE-OSP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSP-FILE.

      *=================================================================
      *    LIBLP/LPSS1RN --- LAST UPDATED: 2016-0404 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPSS1RN
       OPEN-OSS1-FILE.
           PERFORM OPEN-IT.
           MOVE OSS-PATH TO E-FILE.
           OPEN INPUT OSS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSS1-FILE.
           IF IO-FG = 7
              CLOSE OSS-FILE
              GO TO OPEN-OSS1-FILE.
       READ-OSS1-FILE.
           PERFORM READ-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           READ OSS-FILE.
           IF IO-FG = 8
              GO TO READ-OSS1-FILE.
       READ-OSS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           READ OSS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSS1-FILE-NEXT.
       START-OSS1-FILE.
           PERFORM START-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           START OSS-FILE KEY NOT < OSS1-KEY.
       START-OSS1-FILE-GREATER.
           PERFORM START-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           START OSS-FILE KEY > OSS1-KEY.
           UNLOCK OSS-FILE.
       CLOSE-OSS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSS-FILE.

      *=================================================================
      *    LIBLP/LPST1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPST1RN
       OPEN-OST1-FILE.
           PERFORM OPEN-IT.
           MOVE OST-PATH TO E-FILE.
           OPEN INPUT OST-FILE.
           IF IO-FG = 8
              GO TO OPEN-OST1-FILE.
           IF IO-FG = 7
              CLOSE OST-FILE
              GO TO OPEN-OST1-FILE.
       READ-OST1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OST-PATH TO E-FILE.
           MOVE OST1-KEY TO E-KEYX.
           READ OST-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OST1-FILE-NEXT.
       START-OST1-FILE.
           PERFORM START-IT.
           MOVE OST-PATH TO E-FILE.
           MOVE OST1-KEY TO E-KEYX.
           START OST-FILE KEY NOT < OST1-KEY.
       CLOSE-OST1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OST-FILE.

      *=================================================================
      *    LIBLP/LPSU1RN --- LAST UPDATED: 2015-1021 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPSU1IN
       OPEN-OSU1-FILE.
           PERFORM OPEN-IT.
           MOVE OSU-PATH TO E-FILE.
           OPEN I-O OSU-FILE.
           IF IO-FG = 8
             GO TO OPEN-OSU1-FILE.
           IF IO-FG = 7
             CLOSE OSU-FILE
             GO TO OPEN-OSU1-FILE.
           UNLOCK OSU-FILE.
       READ-OSU1-FILE.
           PERFORM READ-IT.
           MOVE OSU-PATH TO E-FILE.
           MOVE OSU1-KEY TO E-KEYX.
           READ OSU-FILE.
           IF IO-FG = 8
              GO TO READ-OSU1-FILE.
       CLOSE-OSU1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSU-FILE.

       OPEN-OTRW-INFO-FILE.
           PERFORM OPEN-IT.
           MOVE OTRW-INFO-PATH TO E-FILE.
           OPEN INPUT OTRW-INFO-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTRW-INFO-FILE.
           IF IO-FG = 7
              CLOSE OTRW-INFO-FILE
              GO TO OPEN-OTRW-INFO-FILE.
       READ-OTRW-INFO-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTRW-INFO-PATH TO E-FILE.
           MOVE OTRW-INFO-KEY TO E-KEYX.
           READ OTRW-INFO-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTRW-INFO-FILE-NEXT.
       START-OTRW-INFO-FILE.
           PERFORM START-IT.
           MOVE OTRW-INFO-PATH TO E-FILE.
           MOVE OTRW-INFO-KEY TO E-KEYX.
           START OTRW-INFO-FILE KEY NOT < OTRW-INFO-KEY.

       CLOSE-OTRW-INFO-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTRW-INFO-FILE.

      *=================================================================
      *    LIBLP/LPTZ1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBLP/LPTZ1RN
       OPEN-OTZ1-FILE.
           PERFORM OPEN-IT.
           MOVE OTZ-PATH TO E-FILE.
           OPEN INPUT OTZ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTZ1-FILE.
           IF IO-FG = 7
              CLOSE OTZ-FILE
              GO TO OPEN-OTZ1-FILE.
       READ-OTZ1-FILE.
           PERFORM READ-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           READ OTZ-FILE.
           IF IO-FG = 8
              GO TO READ-OTZ1-FILE.
       READ-OTZ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           READ OTZ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTZ1-FILE-NEXT.
       START-OTZ1-FILE.
           PERFORM START-IT.
           MOVE OTZ-PATH TO E-FILE.
           MOVE OTZ1-KEY TO E-KEYX.
           START OTZ-FILE KEY NOT < OTZ1-KEY.
       CLOSE-OTZ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTZ-FILE.

      *=================================================================
      *    LIBSP/SPCP1RN --- LAST UPDATED: 2016-0406 JAY
      *=================================================================
      * COPYMEMBER: LIBSP/SPCP1RN
       OPEN-OCP1-FILE.
           PERFORM OPEN-IT.
           MOVE OCP-PATH TO E-FILE.
           OPEN INPUT OCP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCP1-FILE.
           IF IO-FG = 7
              CLOSE OCP-FILE
              GO TO OPEN-OCP1-FILE.
       READ-OCP1-FILE.
           PERFORM READ-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           READ OCP-FILE.
           IF IO-FG = 8
              GO TO READ-OCP1-FILE.
       READ-OCP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           READ OCP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCP1-FILE-NEXT.
       START-OCP1-FILE.
           PERFORM START-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           START OCP-FILE KEY NOT < OCP1-KEY.
       CLOSE-OCP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCP-FILE.

      *=================================================================
      *    LIBSP/SPEP1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBSP/SPEP1RN
      * OPEN-OEP1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OEP-PATH TO E-FILE.
      *     OPEN INPUT OEP-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-OEP1-FILE.
      *     IF IO-FG = 7
      *        CLOSE OEP-FILE
      *        GO TO OPEN-OEP1-FILE.
      * READ-OEP1-FILE.
      *     PERFORM READ-IT.
      *     MOVE OEP-PATH TO E-FILE.
      *     MOVE OEP1-KEY TO E-KEYX.
      *     READ OEP-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-OEP1-FILE.
      * READ-OEP1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OEP-PATH TO E-FILE.
      *     MOVE OEP1-KEY TO E-KEYX.
      *     READ OEP-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-OEP1-FILE-NEXT.
      * START-OEP1-FILE.
      *     PERFORM START-IT.
      *     MOVE OEP-PATH TO E-FILE.
      *     MOVE OEP1-KEY TO E-KEYX.
      *     START OEP-FILE KEY NOT < OEP1-KEY.
      * CLOSE-OEP1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OEP-FILE.

      *=================================================================
      *    LIBSP/SPEW1RN --- LAST UPDATED: 2015-0724 JAY
      *=================================================================
      * COPYMEMBER: LIBSP/SPEW1RN
      * OPEN-OEW1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OEW-PATH TO E-FILE.
      *     OPEN INPUT OEW-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-OEW1-FILE.
      *     IF IO-FG = 7
      *        CLOSE OEW-FILE
      *        GO TO OPEN-OEW1-FILE.
      * READ-OEW1-FILE.
      *     PERFORM READ-IT.
      *     MOVE OEW-PATH TO E-FILE.
      *     MOVE OEW1-KEY TO E-KEYX.
      *     READ OEW-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-OEW1-FILE.
      * READ-OEW1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OEW-PATH TO E-FILE.
      *     MOVE OEW1-KEY TO E-KEYX.
      *     READ OEW-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-OEW1-FILE-NEXT.
      * START-OEW1-FILE.
      *     PERFORM START-IT.
      *     MOVE OEW-PATH TO E-FILE.
      *     MOVE OEW1-KEY TO E-KEYX.
      *     START OEW-FILE KEY NOT < OEW1-KEY.
      * CLOSE-OEW1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OEW-FILE.

      *=================================================================
      *    LIBSP/SPWR1RN --- LAST UPDATED: 041516 CS           
      *=================================================================
      * COPYMEMBER: LIBSP/SPWR1RN
       OPEN-OWR1-FILE.
           PERFORM OPEN-IT.
           MOVE OWR-PATH TO E-FILE.
           OPEN INPUT OWR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OWR1-FILE.
           IF IO-FG = 7
              CLOSE OWR-FILE
              GO TO OPEN-OWR1-FILE.
       READ-OWR1-FILE.
           PERFORM READ-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           READ OWR-FILE.
           IF IO-FG = 8
              GO TO READ-OWR1-FILE.
       READ-OWR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           READ OWR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OWR1-FILE-NEXT.
       START-OWR1-FILE.
           PERFORM START-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           START OWR-FILE KEY NOT < OWR1-KEY.
       CLOSE-OWR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OWR-FILE.

      *=================================================================
       OPEN-OAU1-FILE.
           PERFORM OPEN-IT.
           MOVE OAU-PATH TO E-FILE.
           OPEN INPUT OAU-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAU1-FILE.
           IF IO-FG = 7
              CLOSE OAU-FILE
              GO TO OPEN-OAU1-FILE.
       READ-OAU1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAU-PATH TO E-FILE.
           MOVE OAU1-KEY TO E-KEYX.
           READ OAU-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAU1-FILE-NEXT.
       START-OAU1-FILE.
           PERFORM START-IT.
           MOVE OAU-PATH TO E-FILE.
           MOVE OAU1-KEY TO E-KEYX.
           START OAU-FILE KEY NOT < OAU1-KEY.
       CLOSE-OAU1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAU-FILE.

      *=================================================================
       OPEN-OII-FILE.
           PERFORM OPEN-IT.
           MOVE OII-PATH TO E-FILE.
           OPEN INPUT OII-FILE.
           IF IO-FG = 8
             GO TO OPEN-OII-FILE.
           IF IO-FG = 7
             CLOSE OII-FILE
             GO TO OPEN-OII-FILE.
       READ-OII-FILE.
           PERFORM READ-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           READ OII-FILE.
           IF IO-FG = 8
             GO TO READ-OII-FILE.
       READ-OII-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           READ OII-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OII-FILE-NEXT.
       START-OII-FILE.
           PERFORM START-IT.
           MOVE OII-PATH TO E-FILE.
           MOVE OII-KEY TO E-KEYX.
           START OII-FILE KEY NOT < OII-KEY.
       CLOSE-OII-FILE.
           PERFORM CLOSE-IT.
           CLOSE OII-FILE.

      *=================================================================
       OPEN-OIR-FILE.
           PERFORM OPEN-IT.
           MOVE OIR-PATH TO E-FILE.
           OPEN INPUT OIR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIR-FILE.
           IF IO-FG = 7
              CLOSE OIR-FILE
              GO TO OPEN-OIR-FILE.
       READ-OIR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           READ OIR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIR-FILE-NEXT.
       START-OIR-FILE.
           PERFORM START-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           START OIR-FILE KEY NOT < OIR-KEY.
       CLOSE-OIR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIR-FILE.

      *=================================================================
      * OPEN-ORB1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE ORB-PATH TO E-FILE.
      *     OPEN INPUT ORB-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-ORB1-FILE.
      *     IF IO-FG = 7
      *        CLOSE ORB-FILE
      *        GO TO OPEN-ORB1-FILE.
      * READ-ORB1-FILE.
      *     PERFORM READ-IT.
      *     MOVE ORB-PATH TO E-FILE.
      *     MOVE ORB1-KEY TO E-KEYX.
      *     READ ORB-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-ORB1-FILE.
      * READ-ORB1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE ORB-PATH TO E-FILE.
      *     MOVE ORB1-KEY TO E-KEYX.
      *     READ ORB-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-ORB1-FILE-NEXT.
      * START-ORB1-FILE.
      *     PERFORM START-IT.
      *     MOVE ORB-PATH TO E-FILE.
      *     MOVE ORB1-KEY TO E-KEYX.
      *     START ORB-FILE KEY NOT < ORB1-KEY.
      * CLOSE-ORB1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE ORB-FILE.

      *=================================================================
      * LOCALS;
      *=================================================================
       OPEN-OBW1-FILE.
           PERFORM OPEN-IT.
           MOVE OBW-PATH TO E-FILE.
           OPEN INPUT OBW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBW1-FILE.
           IF IO-FG = 7
              CLOSE OBW-FILE
              GO TO OPEN-OBW1-FILE.
       READ-OBW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           READ OBW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBW1-FILE-NEXT.
       START-OBW1-FILE.
           PERFORM START-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           START OBW-FILE KEY NOT < OBW1-KEY.
       CLOSE-OBW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBW-FILE.

      *=================================================================
       OPEN-OBM1-FILE.
           PERFORM OPEN-IT.
           MOVE OBM-PATH TO E-FILE.
           OPEN INPUT OBM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBM1-FILE.
           IF IO-FG = 7
              CLOSE OBM-FILE
              GO TO OPEN-OBM1-FILE.
       READ-OBM1-FILE.
           PERFORM READ-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           READ OBM-FILE.
           IF IO-FG = 8
              GO TO READ-OBM1-FILE.
       READ-OBM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           READ OBM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBM1-FILE-NEXT.
       START-OBM1-FILE.
           PERFORM START-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           START OBM-FILE KEY NOT < OBM1-KEY.
       CLOSE-OBM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBM-FILE.

      *=================================================================
       OPEN-OBX1-FILE.
           PERFORM OPEN-IT.
           MOVE OBX-PATH TO E-FILE.
           OPEN INPUT OBX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBX1-FILE.
           IF IO-FG = 7
              CLOSE OBX-FILE
              GO TO OPEN-OBX1-FILE.
       READ-OBX1-FILE.
           PERFORM READ-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           READ OBX-FILE.
           IF IO-FG = 8
              GO TO READ-OBX1-FILE.
       START-OBX1-FILE.
           PERFORM START-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           START OBX-FILE KEY NOT < OBX1-KEY.
       CLOSE-OBX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBX-FILE.

      *=================================================================
       OPEN-OCA1-FILE.
           PERFORM OPEN-IT.
           MOVE OCA-PATH TO E-FILE.
           OPEN INPUT OCA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCA1-FILE.
           IF IO-FG = 7
              CLOSE OCA-FILE
              GO TO OPEN-OCA1-FILE.
       READ-OCA1-FILE-NEXT.
           MOVE 0 TO OCA-TILLNO-ZERO.
           PERFORM READ-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           READ OCA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCA1-FILE-NEXT.
       START-OCA1-FILE.
           PERFORM START-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           START OCA-FILE KEY NOT < OCA1-KEY.
       CLOSE-OCA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCA-FILE.

      *=================================================================
       OPEN-OCI1-FILE.
           PERFORM OPEN-IT.
           MOVE OCI-PATH TO E-FILE.
           OPEN INPUT OCI-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCI1-FILE.
           IF IO-FG = 7
              CLOSE OCI-FILE
              GO TO OPEN-OCI1-FILE.
       READ-OCI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           READ OCI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCI1-FILE-NEXT.
       START-OCI1-FILE.
           PERFORM START-IT.
           MOVE OCI-PATH TO E-FILE.
           MOVE OCI1-KEY TO E-KEYX.
           START OCI-FILE KEY NOT < OCI1-KEY.
       CLOSE-OCI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCI-FILE.

      *=================================================================
       OPEN-OCJ1-FILE.
           PERFORM OPEN-IT.
           MOVE OCJ-PATH TO E-FILE.
           OPEN INPUT OCJ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCJ1-FILE.
           IF IO-FG = 7
              CLOSE OCJ-FILE
              GO TO OPEN-OCJ1-FILE.
       READ-OCJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           READ OCJ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCJ1-FILE-NEXT.
       START-OCJ1-FILE.
           PERFORM START-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           START OCJ-FILE KEY NOT < OCJ1-KEY.
       CLOSE-OCJ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCJ-FILE.

      *=================================================================
       OPEN-OCK1-FILE.
           PERFORM OPEN-IT.
           MOVE OCK-PATH TO E-FILE.
           OPEN INPUT OCK-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCK1-FILE.
           IF IO-FG = 7
              CLOSE OCK-FILE
              GO TO OPEN-OCK1-FILE.
       READ-OCK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           READ OCK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCK1-FILE-NEXT.
       START-OCK1-FILE.
           PERFORM START-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           START OCK-FILE KEY NOT < OCK1-KEY.
       CLOSE-OCK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCK-FILE.

      *=================================================================
      * COPYMEMBER: LIBLP/LPCX1RN
       OPEN-OCX1-FILE.
           PERFORM OPEN-IT.
           MOVE OCX-PATH TO E-FILE.
           OPEN INPUT OCX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCX1-FILE.
           IF IO-FG = 7
              CLOSE OCX-FILE
              GO TO OPEN-OCX1-FILE.
       READ-OCX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCX-PATH TO E-FILE.
           MOVE OCX1-KEY TO E-KEYX.
           READ OCX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCX1-FILE-NEXT.
       START-OCX1-FILE.
           PERFORM START-IT.
           MOVE OCX-PATH TO E-FILE.
           MOVE OCX1-KEY TO E-KEYX.
           START OCX-FILE KEY NOT < OCX1-KEY.
       CLOSE-OCX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCX-FILE.

      *=================================================================
       OPEN-ODL1-FILE.
           PERFORM OPEN-IT.
           MOVE ODL-PATH TO E-FILE.
           OPEN INPUT ODL-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODL1-FILE.
           IF IO-FG = 7
              CLOSE ODL-FILE
              GO TO OPEN-ODL1-FILE.
       READ-ODL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODL-PATH TO E-FILE.
           MOVE ODL1-KEY TO E-KEYX.
           READ ODL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODL1-FILE-NEXT.
       START-ODL1-FILE.
           PERFORM START-IT.
           MOVE ODL-PATH TO E-FILE.
           MOVE ODL1-KEY TO E-KEYX.
           START ODL-FILE KEY NOT < ODL1-KEY.
       CLOSE-ODL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODL-FILE.

      *=================================================================
      * OPEN-ODM1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE ODM-PATH TO E-FILE.
      *     OPEN INPUT ODM-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-ODM1-FILE.
      *     IF IO-FG = 7
      *        CLOSE ODM-FILE
      *        GO TO OPEN-ODM1-FILE.
      * READ-ODM1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE ODM-PATH TO E-FILE.
      *     MOVE ODM1-KEY TO E-KEYX.
      *     READ ODM-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-ODM1-FILE-NEXT.
      * START-ODM1-FILE.
      *     PERFORM START-IT.
      *     MOVE ODM-PATH TO E-FILE.
      *     MOVE ODM1-KEY TO E-KEYX.
      *     START ODM-FILE KEY NOT < ODM1-KEY.
      * CLOSE-ODM1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE ODM-FILE.

      *=================================================================
       OPEN-ODT1-FILE.
           PERFORM OPEN-IT.
           MOVE ODT-PATH TO E-FILE.
           OPEN INPUT ODT-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODT1-FILE.
           IF IO-FG = 7
              CLOSE ODT-FILE
              GO TO OPEN-ODT1-FILE.
       READ-ODT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODT-PATH TO E-FILE.
           MOVE ODT1-KEY TO E-KEYX.
           READ ODT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODT1-FILE-NEXT.
       START-ODT1-FILE.
           PERFORM START-IT.
           MOVE ODT-PATH TO E-FILE.
           MOVE ODT1-KEY TO E-KEYX.
           START ODT-FILE KEY NOT < ODT1-KEY.
       CLOSE-ODT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODT-FILE.

      *=================================================================
      * OPEN-OES1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OES-PATH TO E-FILE.
      *     OPEN INPUT OES-FILE.
      *     IF ( IO-FG = 8 )
      *       GO TO OPEN-OES1-FILE.
      *     IF ( IO-FG = 7 )
      *       CLOSE OES-FILE
      *       GO TO OPEN-OES1-FILE.
      *     UNLOCK OES-FILE.
      * READ-OES1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OES-PATH TO E-FILE.
      *     MOVE OES1-KEY TO E-KEYX.
      *     READ OES-FILE NEXT.
      *     IF ( IO-FG = 8 )
      *       GO TO READ-OES1-FILE-NEXT.
      * START-OES1-FILE.
      *     PERFORM START-IT.
      *     MOVE OES-PATH TO E-FILE.
      *     MOVE OES1-KEY TO E-KEYX.
      *     START OES-FILE KEY NOT < OES1-KEY.
      *     UNLOCK OES-FILE.
      * CLOSE-OES1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OES-FILE.

      *=================================================================
       OPEN-OGB-FILE.
           PERFORM OPEN-IT.
           MOVE OGB-PATH TO E-FILE.
           OPEN INPUT OGB-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGB-FILE.
           IF IO-FG = 7
              CLOSE OGB-FILE
              GO TO OPEN-OGB-FILE.
           UNLOCK OGB-FILE.
       READ-OGB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGB-PATH TO E-FILE.
           MOVE OGB-KEY TO E-KEYX.
           READ OGB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGB-FILE-NEXT.
           UNLOCK OGB-FILE.
       START-OGB-FILE.
           PERFORM START-IT.
           MOVE OGB-PATH TO E-FILE.
           MOVE OGB-KEY TO E-KEYX.
           START OGB-FILE KEY NOT < OGB-KEY.
           UNLOCK OGB-FILE.
       CLOSE-OGB-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGB-FILE.

      *=================================================================
       OPEN-OIC1-FILE.
           PERFORM OPEN-IT.
           MOVE OIC-PATH TO E-FILE.
           OPEN INPUT OIC-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIC1-FILE.
           IF IO-FG = 7
              CLOSE OIC-FILE
              GO TO OPEN-OIC1-FILE.
       READ-OIC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIC-PATH TO E-FILE.
           MOVE OIC1-KEY TO E-KEYX.
           READ OIC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIC1-FILE-NEXT.
       START-OIC1-FILE.
           PERFORM START-IT.
           MOVE OIC-PATH TO E-FILE.
           MOVE OIC1-KEY TO E-KEYX.
           START OIC-FILE KEY NOT < OIC1-KEY.
       CLOSE-OIC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIC-FILE.

      *=================================================================
       OPEN-OIX1-FILE.
           PERFORM OPEN-IT.
           MOVE OIX-PATH TO E-FILE.
           OPEN INPUT OIX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIX1-FILE.
           IF IO-FG = 7
              CLOSE OIX-FILE
              GO TO OPEN-OIX1-FILE.
       READ-OIX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIX-PATH TO E-FILE.
           MOVE OIX1-KEY TO E-KEYX.
           READ OIX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIX1-FILE-NEXT.
       START-OIX1-FILE.
           PERFORM START-IT.
           MOVE OIX-PATH TO E-FILE.
           MOVE OIX1-KEY TO E-KEYX.
           START OIX-FILE KEY NOT < OIX1-KEY.
       CLOSE-OIX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIX-FILE.

      *=================================================================
       OPEN-OLE1-FILE.
           PERFORM OPEN-IT.
           MOVE OLE-PATH TO E-FILE.
           OPEN INPUT OLE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLE1-FILE.
           IF IO-FG = 7
              CLOSE OLE-FILE
              GO TO OPEN-OLE1-FILE.
       READ-OLE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           READ OLE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLE1-FILE-NEXT.
       START-OLE1-FILE.
           PERFORM START-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           START OLE-FILE KEY NOT < OLE1-KEY.
       CLOSE-OLE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLE-FILE.

      *=================================================================
       OPEN-OLH1-FILE.
           PERFORM OPEN-IT.
           MOVE OLH-PATH TO E-FILE.
           OPEN INPUT OLH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLH1-FILE.
           IF IO-FG = 7
              CLOSE OLH-FILE
              GO TO OPEN-OLH1-FILE.
       READ-OLH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           READ OLH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLH1-FILE-NEXT.
       START-OLH1-FILE.
           PERFORM START-IT.
           MOVE OLH-PATH TO E-FILE.
           MOVE OLH1-KEY TO E-KEYX.
           START OLH-FILE KEY NOT < OLH1-KEY.
       CLOSE-OLH1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLH-FILE.

      *=================================================================
       OPEN-OLM1-FILE.
           PERFORM OPEN-IT.
           MOVE OLM-PATH TO E-FILE.
           OPEN INPUT OLM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLM1-FILE.
           IF IO-FG = 7
              CLOSE OLM-FILE
              GO TO OPEN-OLM1-FILE.
       READ-OLM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           READ OLM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLM1-FILE-NEXT.
       START-OLM1-FILE.
           PERFORM START-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           START OLM-FILE KEY NOT < OLM1-KEY.
       CLOSE-OLM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLM-FILE.

      *=================================================================
       OPEN-OLN1-FILE.
           PERFORM OPEN-IT.
           MOVE OLN-PATH TO E-FILE.
           OPEN INPUT OLN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLN1-FILE.
           IF IO-FG = 7
              CLOSE OLN-FILE
              GO TO OPEN-OLN1-FILE.
       READ-OLN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           READ OLN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLN1-FILE-NEXT.
       START-OLN1-FILE.
           PERFORM START-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           START OLN-FILE KEY NOT < OLN1-KEY.
       CLOSE-OLN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLN-FILE.

      *=================================================================
       OPEN-OLP1-FILE.
           PERFORM OPEN-IT.
           MOVE OLP-PATH TO E-FILE.
           OPEN INPUT OLP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLP1-FILE.
           IF IO-FG = 7
              CLOSE OLP-FILE
              GO TO OPEN-OLP1-FILE.
       READ-OLP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           READ OLP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLP1-FILE-NEXT.
       START-OLP1-FILE.
           PERFORM START-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           START OLP-FILE KEY NOT < OLP1-KEY.
       CLOSE-OLP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLP-FILE.

      *=================================================================
      * COPYMEMBER: LIBLP/LPLS1RN
       OPEN-OLS1-FILE.
           PERFORM OPEN-IT.
           MOVE OLS-PATH TO E-FILE.
           OPEN INPUT OLS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLS1-FILE.
           IF IO-FG = 7
              CLOSE OLS-FILE
              GO TO OPEN-OLS1-FILE.
       READ-OLS1-FILE.
           PERFORM READ-IT.
           MOVE OLS-PATH TO E-FILE.
           MOVE OLS1-KEY TO E-KEYX.
           READ OLS-FILE.
           IF IO-FG = 8
              GO TO READ-OLS1-FILE.
       READ-OLS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLS-PATH TO E-FILE.
           MOVE OLS1-KEY TO E-KEYX.
           READ OLS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLS1-FILE-NEXT.
       START-OLS1-FILE.
           PERFORM START-IT.
           MOVE OLS-PATH TO E-FILE.
           MOVE OLS1-KEY TO E-KEYX.
           START OLS-FILE KEY NOT < OLS1-KEY.
       CLOSE-OLS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLS-FILE.

      *=================================================================
       GET-OLT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (OLN-ACCTNO = OLT-ACCTNO AND OLT-CODE = "P")
                   MOVE OLN-ACCTNO TO OLT-ACCTNO
                   MOVE "P" TO OLT-CODE
                   MOVE 1 TO OLT-SEQNO
                   PERFORM OPEN-OLT1-FILE
                   PERFORM READ-OLT1-FILE
                   PERFORM CLOSE-OLT1-FILE.
       OPEN-OLT1-FILE.
           PERFORM OPEN-IT.
           MOVE OLT-PATH TO E-FILE.
           OPEN INPUT OLT-FILE.
           IF IO-FG = 8
             GO TO OPEN-OLT1-FILE.
           IF IO-FG = 7
             CLOSE OLT-FILE
             GO TO OPEN-OLT1-FILE.
       READ-OLT1-FILE.
           PERFORM READ-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           READ OLT-FILE.
           IF IO-FG = 8
              GO TO READ-OLT1-FILE.
       READ-OLT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           READ OLT-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLT1-FILE-NEXT.
       START-OLT1-FILE.
           PERFORM START-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           START OLT-FILE KEY NOT < OLT1-KEY.
       CLOSE-OLT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLT-FILE.

      *=================================================================
       OPEN-OLX1-FILE.
           PERFORM OPEN-IT.
           MOVE OLX-PATH TO E-FILE.
           OPEN INPUT OLX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLX1-FILE.
           IF IO-FG = 7
              CLOSE OLX-FILE
              GO TO OPEN-OLX1-FILE.
       READ-OLX1-FILE.
           PERFORM READ-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           READ OLX-FILE.
           IF IO-FG = 8
              GO TO READ-OLX1-FILE.
       READ-OLX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           READ OLX-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLX1-FILE-NEXT.
       START-OLX1-FILE.
           PERFORM START-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           START OLX-FILE KEY NOT < OLX1-KEY.
       CLOSE-OLX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLX-FILE.

      *=================================================================
      * COPYMEMBER: LIBLP/LPML1RN
      *OPEN-OML1-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE OML-PATH TO E-FILE.
      *    OPEN INPUT OML-FILE.
      *    IF IO-FG = 8
      *       GO TO OPEN-OML1-FILE.
      *    IF IO-FG = 7
      *       CLOSE OML-FILE
      *       GO TO OPEN-OML1-FILE.
      *READ-OML1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OML-PATH TO E-FILE.
      *    MOVE OML1-KEY TO E-KEYX.
      *    READ OML-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OML1-FILE.
      *READ-OML1-FILE-NEXT.
      *    PERFORM READ-IT.
      *    MOVE OML-PATH TO E-FILE.
      *    MOVE OML1-KEY TO E-KEYX.
      *    READ OML-FILE NEXT.
      *    IF IO-FG = 8
      *       GO TO READ-OML1-FILE-NEXT.
      *START-OML1-FILE.
      *    PERFORM START-IT.
      *    MOVE OML-PATH TO E-FILE.
      *    MOVE OML1-KEY TO E-KEYX.
      *    START OML-FILE KEY NOT < OML1-KEY.
      *CLOSE-OML1-FILE.
      *    PERFORM CLOSE-IT.
      *    CLOSE OML-FILE.

      *=================================================================
      * COPYMEMBER: LIBLP/LPMP1RN
      *OPEN-OMP1-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    OPEN INPUT OMP-FILE.
      *    IF IO-FG = 8
      *       GO TO OPEN-OMP1-FILE.
      *    IF IO-FG = 7
      *       CLOSE OMP-FILE
      *       GO TO OPEN-OMP1-FILE.
      *READ-OMP1-FILE-NEXT.
      *    PERFORM READ-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    MOVE OMP1-KEY TO E-KEYX.
      *    READ OMP-FILE NEXT.
      *    IF IO-FG = 8
      *       GO TO READ-OMP1-FILE-NEXT.
      *START-OMP1-FILE.
      *    PERFORM START-IT.
      *    MOVE OMP-PATH TO E-FILE.
      *    MOVE OMP1-KEY TO E-KEYX.
      *    START OMP-FILE KEY NOT < OMP1-KEY.
      *CLOSE-OMP1-FILE.
      *    PERFORM CLOSE-IT.
      *    CLOSE OMP-FILE.

      *=================================================================
      * OPEN-OPC1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OPC-PATH TO E-FILE.
      *     OPEN INPUT OPC-FILE.
      *     IF IO-FG = 8
      *       GO TO OPEN-OPC1-FILE.
      *     IF IO-FG = 7
      *       CLOSE OPC-FILE
      *       GO TO OPEN-OPC1-FILE.
      *     UNLOCK OPC-FILE.
      * READ-OPC1-FILE.
      *     PERFORM READ-IT.
      *     MOVE OPC-PATH TO E-FILE.
      *     MOVE OPC1-KEY TO E-KEYX.
      *     READ OPC-FILE.
      *     IF IO-FG = 8
      *       GO TO READ-OPC1-FILE.
      *     UNLOCK OPC-FILE.
      * READ-OPC1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OPC-PATH TO E-FILE.
      *     MOVE OPC1-KEY TO E-KEYX.
      *     READ OPC-FILE NEXT.
      *     IF IO-FG = 8
      *       GO TO READ-OPC1-FILE-NEXT.
      * START-OPC1-FILE.
      *     PERFORM START-IT.
      *     MOVE OPC-PATH TO E-FILE.
      *     MOVE OPC1-KEY TO E-KEYX.
      *     START OPC-FILE KEY NOT < OPC1-KEY.
      * CLOSE-OPC1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OPC-FILE.

      *=================================================================
      * OPEN-OPD1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE OPD-PATH TO E-FILE.
      *     OPEN INPUT OPD-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-OPD1-FILE.
      *     IF IO-FG = 7
      *        CLOSE OPD-FILE
      *        GO TO OPEN-OPD1-FILE.
      * READ-OPD1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE OPD-PATH TO E-FILE.
      *     MOVE OPD1-KEY TO E-KEYX.
      *     READ OPD-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-OPD1-FILE-NEXT.
      * START-OPD1-FILE.
      *     PERFORM START-IT.
      *     MOVE OPD-PATH TO E-FILE.
      *     MOVE OPD1-KEY TO E-KEYX.
      *     START OPD-FILE KEY NOT < OPD1-KEY.
      * CLOSE-OPD1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE OPD-FILE.

      *=================================================================
      * COPYMEMBER: LIBLP/LPRX1RN
       OPEN-ORX1-FILE.
           PERFORM OPEN-IT.
           MOVE ORX-PATH TO E-FILE.
           OPEN INPUT ORX-FILE.
           IF ( IO-FG = 8 )
             GO TO OPEN-ORX1-FILE.
           IF ( IO-FG = 7 )
             CLOSE ORX-FILE
             GO TO OPEN-ORX1-FILE.
           UNLOCK ORX-FILE.
       READ-ORX1-FILE.
           PERFORM READ-IT.
           MOVE ORX-PATH TO E-FILE.
           PERFORM SET-ORX1-E-KEYX.
           READ ORX-FILE.
           IF ( IO-FG = 8 )
              GO TO READ-ORX1-FILE.
       READ-ORX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ORX-PATH TO E-FILE.
           PERFORM SET-ORX1-E-KEYX.
           READ ORX-FILE NEXT.
           IF ( IO-FG = 8 )
             GO TO READ-ORX1-FILE-NEXT.
       START-ORX1-FILE.
           PERFORM START-IT.
           MOVE ORX-PATH TO E-FILE.
           PERFORM SET-ORX1-E-KEYX.
           START ORX-FILE KEY NOT < ORX1-KEY.
           UNLOCK ORX-FILE.
       CLOSE-ORX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ORX-FILE.
       SET-ORX1-E-KEYX.
           MOVE ORX-ACCTNO    TO E-KEYX.

      *=================================================================
       OPEN-OTY1-FILE.
           PERFORM OPEN-IT.
           MOVE OTY-PATH TO E-FILE.
           OPEN INPUT OTY-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTY1-FILE.
           IF IO-FG = 7
              CLOSE OTY-FILE
              GO TO OPEN-OTY1-FILE.
       READ-OTY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           READ OTY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTY1-FILE-NEXT.
       START-OTY1-FILE.
           PERFORM START-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           START OTY-FILE KEY NOT < OTY1-KEY.
       CLOSE-OTY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTY-FILE.

