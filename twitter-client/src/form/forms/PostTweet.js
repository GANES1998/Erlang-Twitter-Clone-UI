import {Form, Formik} from "formik";
import * as yup from "yup"
import {useContext, useEffect, useState} from "react";
import {StaticDataContext} from "../../context/StaticDataContext";
import {postTweet} from "../../service/TweetService";
import TextArea from "../components/TextArea";

const initialValue = {
    tweet: ''
};

const PostTweet = () => {
    const {user} = useContext(StaticDataContext);
    const [tweetCounts, setTweetCounts] = useState(0);
    const [showPosted, setShowPosted] = useState(false);


    useEffect(() => {
        setShowPosted(tweetCounts > 0);

        return () => setTimeout(() => setShowPosted(false), 1000);
    }, [tweetCounts]);

    const validationSchema = yup.object({
        tweet: yup.string().required()
    });

    const handleSubmit = async (values, {resetForm}) => {
        const tweetObject = await postTweet(user, values.tweet);
        setTweetCounts(tw => tw + 1);
        resetForm();
    }

    return (
        <>
            {showPosted && <p>Tweets posted successfully</p>}
            <Formik initialValues={initialValue}
                    validationSchema={validationSchema}
                    onSubmit={handleSubmit}>
                {(formik) => (
                    <Form>
                        <TextArea name={'tweet'} placeholder={'Post your tweet here'}/>
                        <button type={"submit"} disabled={!(formik.dirty && formik.isValid)}>Submit</button>
                    </Form>
                )}
            </Formik>
        </>
    )
}

export default PostTweet;