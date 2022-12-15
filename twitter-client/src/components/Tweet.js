import classes from "./tweet.module.scss";
import {retweet} from "../service/TweetService";
import {useContext} from "react";
import {StaticDataContext} from "../context/StaticDataContext";

const Tweet = ({tweet, userId, tweetId}) => {
    const {user} = useContext(StaticDataContext);

    const handleRetweeted = async () => {
        await retweet(user, tweetId);
    }

    return (
        <div className={classes.tweet}>
            <div className={classes.tweetTopLine}>
                <p>Posted by <i>{userId}</i></p>
                <p><i>{tweetId}</i></p>
            </div>
            <p className={classes.tweetContent}>{tweet}</p>
            <div className={classes.tweetLastLine}>
                <button
                    onClick={(e) => handleRetweeted()}
                    className={classes.retweetButton}>Retweet</button>
            </div>
        </div>
    )
}

export default Tweet;