import Request from "./Request";

export const postTweet = async (userId, tweetContent) => {
    const response = await Request.get('/tweets', {params: {userId, tweet: tweetContent}});
    return response.data.data;
}

export const getMentions = async (userId) => {
    const response = await Request.get("/getmentions", {params: {user: userId}});
    return response.data.data;
}

export const getCurrentUserFeeds = async (userId) => {
    const response = await Request.get("/feeds", {params: {user: userId}});
    return response.data.data;
}

export const getHashtags = async (hashtag) => {
    const response = await Request.get("/searchhashtags", {params: {hashtag: hashtag}});
    return response.data.data;
}

export const retweet = async (user, tweetId) => {
    await Request.get("/retweet", {params: {user, tweetId}});
}