import {useState} from "react";
import Tweet from "../components/Tweet";
import {getHashtags} from "../service/TweetService";
import SearchByHashtagForm from "../form/forms/SearchHashTagForm";

const SearchByHashtag = () => {
    const [hashTags, setHashTags] = useState([]);
    const [loading, setLoading] = useState(false);

    const handleSubmit = async (values, {resetForm}) => {
        setLoading(true);
        const tweets = await getHashtags(values.hashtag);
        setHashTags(tweets);
        setLoading(false);
    }


    return (
        <div>
            <h2>Search By Hashtag</h2>
            <SearchByHashtagForm handleSubmit={handleSubmit} />
            {hashTags.length > 0 && <h3>Hashtags</h3>}
            {hashTags.map((hashtag) => <Tweet {...hashtag} />)}
        </div>
    )
}

export default SearchByHashtag;