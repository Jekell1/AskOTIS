      * COPYMEMBER: LIBFP/PST.COPYXW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.COPYXW
      *
      * DESCRIPTION   :  LASER (PST) FORM COPY LIBRARY WORKERS
      *                  - WITH PAYOFF/REFUNDS
      *
      *=================================================================
      * REV:
      * KEC 2011-1027 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW*** POSTSCRIPT
      *
      * JKC 2021-0608 ADDED ACUSQL ITEMS (CONNECTW_SQL)
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      *
      ******************************************************************
      *
      *    P S T - C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CRNOR2W.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
      *COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/SAFECKW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
      *=================================================================
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *=================================================================

       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPWDELW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".

       COPY "LIBSP/WORDSW.CPY".
