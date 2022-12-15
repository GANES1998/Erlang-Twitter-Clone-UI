import classes from "./navbar.module.scss";
import {useContext} from "react";
import {StaticDataContext} from "../context/StaticDataContext";

const Navbar = () => {
    const {user} = useContext(StaticDataContext);

    return (
        <div className={classes.navbar}>
            <div className={classes.brand}>
                <p className={classes.logotext}>Twitter Clone</p>
            </div>
            {user && <p className={classes.welcomeText}>Welcome, {user}</p>}
        </div>
    )
}

export default Navbar;