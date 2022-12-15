import Request from "./Request";

export const subscribe = async (currentUserId, subscriberUserId) => {
    const response = await Request.get('/subscribe', {params: {user: currentUserId, subscription: subscriberUserId}});
}

