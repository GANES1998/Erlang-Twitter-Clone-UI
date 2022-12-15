import Request from "./Request";

export const postTweet = async (userId, tweetContent) => {
    const response = await Request.post('/tweets', {userId, tweet: tweetContent});
    return response.data;
}

export const getMentions = async (userId) => {
    const response = await Request.get("/tweets/mentions", {params: {user: userId}});
    return response.data;
}

export const getCurrentUserFeeds = async (userId) => {
    const response = await Request.get("/tweets/feeds", {params: {user: userId}});
    return response.data;
}

export const getHashtags = async (hashtag) => {
    const response = await Request.get("/tweets/hashtags", {params: {hashtag: hashtag}});
    return response.data;
}

export const retweet = async (user, tweetId) => {
    await Request.get("/tweets/retweet", {params: {user, tweetId}});
}