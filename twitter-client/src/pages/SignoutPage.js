import {useContext, useEffect, useState} from "react";
import {StaticDataContext} from "../context/StaticDataContext";
import {useLocation, useNavigate} from "react-router";

const SignoutPage = () => {
    const {user, logout} = useContext(StaticDataContext);
    const [signedOut, setSignedOut] = useState(false);
    const location = useLocation();
    const navigate = useNavigate();

    useEffect(() => {
        setTimeout(() => {
                logout();
                setSignedOut(true);
            }
            , 1000)
    }, []);

    if (signedOut) {
        navigate('/signup')
    }

    return (
        <>
            <h3>Signing out...</h3>
        </>
    )

}

export default SignoutPage;