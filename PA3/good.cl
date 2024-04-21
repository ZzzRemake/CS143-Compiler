class A {
ana(): Int {
(let x:Int <- 1 in 2)+3
};
};

Class BB__ inherits A {
    kkwd : String <- "???";
    kknd(): Int {
        {
        let x: Int <- (let y:Int <- 2 in {y <- y + 4;y <- y*5; y;}) in {
            x;
        };
        case x of
            xx : Int => not xx;
            yy : String => isvoid yy;
        esac;
        let wo : Int <- (new A).ana() in {
            wo;
        };
        if isvoid kkwd then
            0
            else
            1
            fi;
        }
    };
};
