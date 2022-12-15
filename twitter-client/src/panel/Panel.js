import classes from "./styles.module.scss";
import {NavLink} from "react-router-dom";
import clsx from "clsx";
import {useContext} from "react";
import {StaticDataContext} from "../context/StaticDataContext";

export const URL_DISPLAY_MAPPING = {
    "Post A Tweet": '/posttweet',
    "Search Hashtags": '/search',
    "Get Feeds": '/feeds',
    "See Mentions": '/mentions',
    "Subscribe": "/subscribe",
    "Signout": "/signout"
}

export const URL_NON_SIGNOUT_MAPPING = {
    "Sign Up": "/signup",
    "Login": "/login"
}

const Panel = () => {
    const {user, isLoggedIn} = useContext(StaticDataContext);

    return (
        <div className={classes.panel}>
            {Object.entries(isLoggedIn ? URL_DISPLAY_MAPPING : URL_NON_SIGNOUT_MAPPING).map(
                ([key, value]) => (
                    <NavLink key={key}
                             className={
                                 (navData) => clsx(classes.panelItem, {[classes.activePanelItem]: navData.isActive})
                             }
                             to={value}>
                        {key}
                    </NavLink>
                ))
            }
        </div>
    )
}

export default Panel;