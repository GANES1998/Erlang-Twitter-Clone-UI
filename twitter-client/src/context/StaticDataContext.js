import {createContext, useState} from "react";

const StaticDataContext = createContext();

const StaticDataContextProvider = ({children}) => {

    const [user, setUser] = useState(undefined);

    const loginUser = (user) => {
        setUser(user);
    }

    const logout = () => {
        setUser(undefined)
    }

    const contextValue = {user, loginUser, logout, isLoggedIn: user !== undefined}


    return (
        <StaticDataContext.Provider value={{...contextValue}}>
            {children}
        </StaticDataContext.Provider>
    )
}

export {StaticDataContextProvider, StaticDataContext};