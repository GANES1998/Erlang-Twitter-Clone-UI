import {useContext, useEffect, useState} from "react";
import {StaticDataContext} from "../context/StaticDataContext";
import {getCurrentUserFeeds} from "../service/TweetService";
import Tweet from "../components/Tweet";

const GetFeeds = () => {
    const {user} = useContext(StaticDataContext);
    const [tweets, setTweets] = useState([]);
    useEffect(() => {
        const getFeeds = async () => {
            const tweets = await getCurrentUserFeeds(user);
            setTweets(tweets);
        }

        getFeeds();
    }, []);

    return (
        <div>
            <h2>Get Feeds</h2>
            {tweets.length === 0 && <h4><i>Sorry No feeds.</i></h4>}
            {tweets.length > 0 && <h3>Tweets</h3>}
            {tweets.map(tweet => <Tweet {...tweet}/>)}
        </div>
    )
}

export default GetFeeds;