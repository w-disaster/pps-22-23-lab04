package u04lab.polyglot.a01a;

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
public interface Logics {

    enum Result {
        HIT, MISS, WON, LOST
    }

    Result hit(int row, int col);

}

