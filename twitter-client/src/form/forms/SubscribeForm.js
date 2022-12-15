import {Form, Formik} from "formik";
import * as yup from "yup"
import {useContext, useEffect, useState} from "react";
import {StaticDataContext} from "../../context/StaticDataContext";
import {postTweet} from "../../service/TweetService";
import TextArea from "../components/TextArea";
import {subscribe} from "../../service/SubscriptionPage";
import InputComponent from "../components/InputComponent";

const initialValue = {
    subscription: ''
};

const SubscribeForm = () => {
    const {user} = useContext(StaticDataContext);
    const [tweetCounts, setTweetCounts] = useState(0);
    const [showPosted, setShowPosted] = useState(false);


    useEffect(() => {
        setShowPosted(tweetCounts > 0);

        return () => setTimeout(() => setShowPosted(false), 1000);
    }, [tweetCounts]);

    const validationSchema = yup.object({
        subscription: yup.string().required()
    });

    const handleSubmit = async (values, {resetForm}) => {
        await subscribe(user, values.subscription);
        setTweetCounts(tw => tw + 1);
        resetForm();
    }

    return (
        <>
            {showPosted && <p>Subscribed successfully</p>}
            <Formik initialValues={initialValue}
                    validationSchema={validationSchema}
                    onSubmit={handleSubmit}>
                {(formik) => (
                    <Form>
                        <InputComponent name={'subscription'} placeholder={'Place your subscription'}/>
                        <button type={"submit"} disabled={!(formik.dirty && formik.isValid)}>Submit</button>
                    </Form>
                )}
            </Formik>
        </>
    )
}

export default SubscribeForm;