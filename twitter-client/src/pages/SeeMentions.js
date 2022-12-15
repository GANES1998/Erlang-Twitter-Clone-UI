import {useContext, useEffect, useState} from "react";
import {getMentions} from "../service/TweetService";
import {StaticDataContext} from "../context/StaticDataContext";
import Tweet from "../components/Tweet";

const GetMentions = () => {
    const {user} = useContext(StaticDataContext);
    const [tweets, setTweets] = useState([]);
    useEffect(() => {
        const getUser = async () => {
            const tweets = await getMentions(user);
            setTweets(tweets);
        }

        getUser();

    }, []);

    return (
        <div>
            <h2>Get Mentions</h2>
            {tweets.length === 0 && <h4><i>Sorry No mentions.</i></h4>}
            {tweets.length > 0 && <h3>Tweets</h3>}
            {tweets.map(tweet => <Tweet {...tweet}/>)}
        </div>
    )
}

export default GetMentions;