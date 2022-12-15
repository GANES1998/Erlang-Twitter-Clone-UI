import classes from "./layout.module.scss";
import Panel from "../panel/Panel";
import {Route, Routes} from "react-router";
import PostTweet from "../pages/PostTweet";
import GetFeeds from "../pages/GetFeeds";
import GetMentions from "../pages/SeeMentions";
import SignUpPage from "../pages/SignUpPage";
import SearchByHashtag from "../pages/SearchByHashtag";
import SignoutPage from "../pages/SignoutPage";
import SubscribeForm from "../form/forms/SubscribeForm";

const MainLayout = ({children}) => (
    <div className={classes.layout}>
        <div className={classes.panel}>
            <Panel/>
        </div>
        <div className={classes.displayPage}>

            <Routes>
                <Route path={"/signup"} element={<SignUpPage/>}/>
                <Route path={"/posttweet"} element={<PostTweet/>}/>
                <Route path={"/search"} element={<SearchByHashtag/>}/>
                <Route path={"/feeds"} element={<GetFeeds/>}/>
                <Route path={"/mentions"} element={<GetMentions/>}/>
                <Route path={"/signout"} element={<SignoutPage />}/>
                <Route path={"/subscribe"} element={<SubscribeForm />}/>
            </Routes>

        </div>
    </div>
);

export default MainLayout;