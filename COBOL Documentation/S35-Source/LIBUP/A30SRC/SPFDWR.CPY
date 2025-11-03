      * COPYMEMBER: LIBSP/SPFDWR
      *************************************************************
      *                     'WR'
      *            ( WORLD RATING SHEET FILE )
      *                    RSZ - ???
      * REV:
      *  JTG 030908 MOVED '78  WR-SIZE FROM SP01WR AND PLACED IN SPFDWR'
      *             SO IT COULD BE USED WITH A 'COPY   REPLACING'
      *  JTG 031023 SET RECORD SIZE WR-SIZE TO 924
      *   CS 160418 MOVED WR-SIZE TO SP01WR
      *************************************************************
       FD  OWR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/SP01WR.CPY" REPLACING LEADING "WR" BY "OWR".
